{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, LambdaCase #-}

module Factor.Trait(Trait(..), ParameterizedTrait(..), TraitInfo(..), UnsatisfiedTrait(..),
                    FromUnsatisfiedTrait(..),
                    mergeTraits, traitAt, nestedTrait, nestedTraitDeep, nestedTraitDeepFunctor,
                    moduleSatisfies, moduleSatisfies',
                    bindTrait, bindTraitUnchecked,
                    makeMinimalModule, bindModule, makeFreshModuleName) where

import Factor.Trait.Types
import Factor.Trait.Argument
import Factor.State.Reader
import Factor.Error
import Factor.Id
import Factor.Util
import Factor.Type
import Factor.Type.Unify
import Factor.State.Resource
import Factor.Code
import Factor.Names
import Factor.State.TypeDecl

import Control.Monad.Reader hiding (reader)
import Control.Monad.Except
import Control.Monad.RWS hiding (reader)
import Control.Monad.State
import Control.Lens
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Maybe

requireSubtype :: (FromUnsatisfiedTrait e, MonadError e m) =>
                  ReadOnlyState -> QId -> TraitInfo -> Type -> Type -> m ()
requireSubtype reader q t a b =
    case evalRWST (a `canUnify` b) reader () of
      Left (_ :: FactorError) -> throwError (fromUnsatisfiedTrait $ IncompatibleWithTrait q t)
      Right ((), _) -> pure ()

requireExists :: (FromUnsatisfiedTrait e, MonadError e m) => QId -> TraitInfo -> Maybe ReaderValue -> m ReaderValue
requireExists q t Nothing = throwError (fromUnsatisfiedTrait $ MissingFromTrait q t)
requireExists _ _ (Just x) = pure x

-- traitDemandsType :: MonadError FactorError m => ReadOnlyState -> Trait -> m Bool
-- traitDemandsType r (Trait xs) = getAny <$> foldMapM go xs
--     where go (_, info) =
--               case info of
--                 TraitFunction {} -> pure $ Any False
--                 TraitMacro {} -> pure $ Any False
--                 TraitModule {} -> pure $ Any False
--                 TraitInclude (TraitRef q args) ->
--                     lookupFn q r >>= \case
--                              TraitValue pt -> do
--                                t <- runReaderT (bindTrait q pt args) r
--                                Any <$> traitDemandsType r t
--                              _ -> throwError (NoSuchTrait q)
--                 TraitDemandType -> pure $ Any True
--                 TraitFunctor {} -> pure $ Any False

-- functorDemandsType :: MonadError FactorError m => ReadOnlyState -> Map Id FunctorInfo -> m Bool
-- functorDemandsType _r m = getAny <$> foldMapM go (Map.toList m)
--     where go (_, info) =
--               case info of
--                 FunctorUDFunction {} -> pure $ Any False
--                 FunctorUDMacro {} -> pure $ Any False
--                 FunctorModule {} -> pure $ Any False
--                 FunctorFunctor {} -> pure $ Any False
--                 FunctorTrait _ -> pure $ Any False
--                 FunctorDemandType -> pure $ Any True

mergeTraits :: Trait -> Trait -> Trait
mergeTraits (Trait xs) (Trait ys) = Trait $ xs ++ ys

-- TODO Handle merges here too
traitAt :: MonadError FactorError m => ReadOnlyState -> Trait -> Id -> m TraitInfo
traitAt r (Trait xs) y = foldMapM check xs >>= \case
                         [] -> throwError (NoSuchTrait (QId [y]))
                         (z:_) -> pure z
    where check (v, info)
              | v == y = pure [info]
              | otherwise = case info of
                              TraitInclude (TraitRef q args) ->
                                  lookupFn q r >>= \case
                                           TraitValue pt -> do
                                             Trait ts <- runReaderT (bindTrait q pt args) r
                                             foldMapM check ts
                                           _ -> throwError (NoSuchTrait q)
                              _ -> pure []

nestedTrait :: MonadError FactorError m => ReadOnlyState -> Trait -> Id -> m Trait
nestedTrait r (Trait xs) y = foldMapM check xs >>= \case
                             [] -> throwError (NoSuchTrait (QId [y]))
                             ys -> pure $ foldl mergeTraits (Trait []) ys
    where check (v, info) =
              case info of
                TraitFunction {} -> pure []
                TraitMacro {} -> pure []
                TraitModule m | v == y -> pure [Trait m]
                              | otherwise -> pure []
                TraitInclude (TraitRef q args) ->
                    lookupFn q r >>= \case
                             TraitValue pt -> do
                               t <- runReaderT (bindTrait q pt args) r
                               result <- nestedTrait r t y
                               return [result]
                             _ -> throwError (NoSuchTrait q)
                -- TODO I guess this one could technically be
                -- meaningful in some cases, if we parameterize
                -- correctly. That would be a big project to make it
                -- work though...
                TraitFunctor {} -> pure []
                TraitType {} -> pure []

nestedTraitDeep :: MonadError FactorError m => ReadOnlyState -> Trait -> QId -> m Trait
nestedTraitDeep r t (QId xs) = foldM (nestedTrait r) t xs

nestedTraitDeepFunctor :: MonadError FactorError m =>
                          ReadOnlyState -> Trait -> QId -> m ([ModuleArg], [(Id, TraitInfo)])
nestedTraitDeepFunctor _ _ (QId []) = throwError (NoSuchTrait (QId []))
nestedTraitDeepFunctor r (Trait ys) (QId xs) = do
  Trait ys' <- nestedTraitDeep r (Trait ys) (QId $ init xs)
  foldMapM check ys' >>= \case
           [] -> throwError (NoSuchTrait (QId xs))
           (y:_) -> pure y -- TODO We just take the first match right
                           -- now, since there's no good way to merge
                           -- functors at the moment.
 where check (v, info) =
           case info of
                TraitFunction {} -> pure []
                TraitMacro {} -> pure []
                TraitModule _ -> pure []
                TraitInclude (TraitRef q args) ->
                    lookupFn q r >>= \case
                             TraitValue pt -> do
                               Trait info' <- runReaderT (bindTrait q pt args) r
                               foldMapM check info'
                             _ -> throwError (NoSuchTrait q)
                TraitFunctor ty zs
                    | v == last xs -> pure [(ty, zs)]
                    | otherwise -> throwError (NoSuchTrait (QId xs))
                TraitType {} -> pure []

-- TODO We want to verify properties like "does this included trait
-- exist" or "did we pass the right number of arguments to the
-- included trait" before requiring the trait in a module.
moduleSatisfies :: MonadError FactorError m => ReadOnlyState -> Trait -> QId -> Module -> m ()
moduleSatisfies reader t0 qid0 m0 = let f v | v == Id "Self" = qid0
                                            | otherwise = QId [v]
                                        Trait reqs0 = substituteTrait f t0
                                    in mapM_ (go qid0 m0) reqs0
    where go qid m (v, info) =
              let qid' = qid <> QId [v]
                  value = (m^.moduleNames.possibly (ix v)) >>= (\rid -> reader^.readerResources.possibly (ix rid))
              in case info of
                   TraitFunction (PolyFunctionType r reqtype) -> do
                             value' <- requireExists qid' info value
                             case value' of
                               UDFunction (PolyFunctionType _ decltype) _ ->
                                   requireSubtype reader qid' info (FunType decltype) (toGround r $ FunType reqtype)
                               BIFunction (PolyFunctionType _ decltype) _ ->
                                   requireSubtype reader qid' info (FunType decltype) (toGround r $ FunType reqtype)
                               _ -> throwError (TraitError $ MissingFromTrait qid' info)
                   TraitMacro (PolyFunctionType r reqtype) -> do
                             value' <- requireExists qid' info value
                             case value' of
                               UDMacro (PolyFunctionType _ decltype) _ -> -- TODO Support BIMacro here,
                                                                          -- once we write that
                                   requireSubtype reader qid' info (FunType decltype) (toGround r $ FunType reqtype)
                               _ -> throwError (TraitError $ MissingFromTrait qid' info)
                   TraitModule reqs -> do
                             value' <- requireExists qid' info value
                             case value' of
                               ModuleValue m' -> mapM_ (go qid' m') reqs
                               _ -> throwError (TraitError $ MissingFromTrait qid' info)
                   TraitInclude (TraitRef q args) ->
                       lookupFn q reader >>= \case
                                TraitValue pt -> do
                                  t <- runReaderT (bindTrait q pt args) reader
                                  moduleSatisfies reader t qid m
                                _ -> throwError (NoSuchTrait q)
                   TraitType n -> do
                             value' <- requireExists qid' info value
                             case value' of
                               TypeValue (TypeData n')
                                   | n == n' -> pure ()
                                   | otherwise -> throwError (TraitError $ IncompatibleWithTrait qid' info)
                               _ -> throwError (TraitError $ MissingFromTrait qid' info)
                   TraitFunctor params info' -> do
                             value' <- requireExists qid' info value
                             ParameterizedModule args modl <-
                                 case value' of
                                   FunctorValue (ParameterizedModule args modl)
                                       | length args == length params ->
                                           pure (ParameterizedModule args modl)
                                   _ -> throwError (TraitError $ MissingFromTrait qid' info)
                             let freshname = makeFreshModuleName "Tmp" reader
                                 -- TODO Right now, trait args are invariant. Ideally,
                                 -- they'd be able to be more general, but for that
                                 -- we need a proper isSubtraitOf function. This
                                 -- current implementation is sound but incredibly
                                 -- strict compared to what it could be.
                                 validateArgs (ModuleArg _ p) (ModuleArg _ a)
                                     | p == a = pure ()
                                     | otherwise = throwError (TraitError $ MissingFromTrait qid info)
                             void $ zipWithM validateArgs params args
                             ((modl', rid, args'), reader') <- runStateT (makeMinimalModule (QId [freshname]) (ParameterizedModule args modl)) reader
                             let reader'' = set (readerNames.at freshname) (Just rid) reader'
                             trait <- runReaderT (bindTrait qid' (ParameterizedTrait params (Trait info')) args') reader''
                             moduleSatisfies reader'' trait qid' modl'

moduleSatisfies' :: (MonadReader ReadOnlyState m, MonadError FactorError m) =>
                    Trait -> QId -> Module -> m ()
moduleSatisfies' t qid m = ask >>= \r -> moduleSatisfies r t qid m

-- Note: Doesn't bind Self (Self will be bound during moduleSatisfies)
bindTrait :: (MonadReader ReadOnlyState m, MonadError FactorError m) =>
             QId -> ParameterizedTrait -> [QId] -> m Trait
bindTrait qid (ParameterizedTrait params trait) args
    | length params /= length args = throwError $ TraitArgError qid (length params) (length args)
    | otherwise = do
        _zipped <- forM (zip params args) $ \(ModuleArg param (TraitRef req innerargs), arg) -> do
                    modl <- ask >>= lookupFn arg >>= \case
                            ModuleValue m -> pure m
                            _ -> throwError (NoSuchModule arg)
                    req' <- ask >>= lookupFn req >>= \case
                            TraitValue pt -> bindTrait req pt innerargs
                                             -- TODO The above case can DEFINITELY cause
                                             -- infinite loop issues in the compiler that
                                             -- we need to detect and err out of.
                            _ -> throwError (NoSuchTrait req)
                    moduleSatisfies' req' arg modl
                    return (param, arg)
        return $ bindTraitUnchecked qid (ParameterizedTrait params trait) args

bindTraitUnchecked :: QId -> ParameterizedTrait -> [QId] -> Trait
bindTraitUnchecked _qid (ParameterizedTrait params trait) args =
    let submap = Map.fromList (zip (fmap (\(ModuleArg param _) -> param) params) args)
        subfn k = maybe (QId [k]) id (Map.lookup k submap)
    in substituteTrait subfn trait

-- Makes a minimal module for a functor, used for type-checking.
makeMinimalModule :: (MonadState ReadOnlyState m, MonadError FactorError m) =>
                     QId -> ParameterizedModule -> m (Module, RId, [QId])
makeMinimalModule qid pm = do
  let ParameterizedModule params _ = pm
  args <- forM params $ \(ModuleArg _ (TraitRef req innerargs)) -> do
                             req' <- get >>= lookupFn req >>= \case
                                     TraitValue pt -> get >>= runReaderT (bindTrait req pt innerargs)
                                     _ -> throwError (NoSuchTrait req)
                             let Id traitname = lastname req
                             modlname <- makeFreshModuleName ("Tmp" ++ traitname) <$> get
                             (_, rid) <- makeMinimalModuleFor (QId [modlname]) req'
                             readerNames.at modlname .= Just rid
                             return (QId [modlname])
  -- TODO Require the functor name as an argument here (requires a bit
  -- of tweaking in Functor.Type.Checker)
  rid <- bindModule qid (QId []) pm args
  get >>= \r -> case r^.readerResources.possibly (ix rid) of
                  Just (ModuleValue m) -> return (m, rid, args)
                  _ -> error "Internal error in makeMinimalModule (unexpected shape after bindModule)"

-- TODO Handle merges (traits with the same name declared multiple
-- times should merge the requirements)
makeMinimalModuleFor :: (MonadState ReadOnlyState m, MonadError FactorError m) =>
                        QId -> Trait -> m (Module, RId)
makeMinimalModuleFor qid (Trait info) = do
  let go modl (i, v) =
          let qid' = qid <> QId [i]
          in case v of
               TraitFunction p -> do
                      rid <- appendResourceRO' qid' (UDFunction p (Function (Just i) unsafeImpl))
                      return $ set (moduleNames.at i) (Just rid) modl
               TraitMacro p -> do
                      rid <- appendResourceRO' qid' (UDMacro p (Macro i unsafeImpl))
                      return $ set (moduleNames.at i) (Just rid) modl
               TraitModule m -> do
                   modlname <- makeFreshModuleName ("Tmp" ++ unId i) <$> get
                   (_, rid) <- makeMinimalModuleFor (QId [modlname]) (Trait m)
                   return $ set (moduleNames.at i) (Just rid) modl
               TraitInclude (TraitRef innername innerargs) -> do
                   innername' <- get >>= lookupFn innername >>= \case
                                 TraitValue pt -> get >>= runReaderT (bindTrait innername pt innerargs)
                                 _ -> throwError (NoSuchTrait innername)
                   let Trait innerinfo = innername'
                   foldM go modl innerinfo
               TraitFunctor args info' -> do
                   inner <- makeMinimalInFunctor qid' (Trait info')
                   rid <- appendResourceRO' qid' (FunctorValue (ParameterizedModule args inner))
                   return $ set (moduleNames.at i) (Just rid) modl
               TraitType n -> do
                   rid <- appendResourceRO' qid' (TypeValue (TypeData n))
                   return $ set (moduleNames.at i) (Just rid) modl
  dat <- foldM go emptyModule info
  rid <- appendResourceRO' qid (ModuleValue dat)
  return (dat, rid)

makeMinimalInFunctor :: (MonadState ReadOnlyState m, MonadError FactorError m) =>
                        QId -> Trait -> m (Map Id FunctorInfo)
makeMinimalInFunctor qid (Trait info) = do
  let go modl (i, v) =
          let qid' = qid <> QId [i]
          in case v of
               TraitFunction p ->
                   pure (Map.insert i (FunctorUDFunction p (Function (Just i) unsafeImpl)) modl)
               TraitMacro p ->
                   pure (Map.insert i (FunctorUDMacro p (Macro i unsafeImpl)) modl)
               TraitModule m -> do
                   inner <- makeMinimalInFunctor qid (Trait m)
                   return (Map.insert i (FunctorModule inner) modl)
               TraitInclude (TraitRef innername innerargs) -> do
                   innername' <- get >>= lookupFn innername >>= \case
                                 TraitValue pt -> get >>= runReaderT (bindTrait innername pt innerargs)
                                 _ -> throwError (NoSuchTrait innername)
                   let Trait innerinfo = innername'
                   foldM go modl innerinfo
               TraitFunctor args info' -> do
                   info'' <- makeMinimalInFunctor qid' (Trait info')
                   return $ Map.insert i (FunctorFunctor args info'') modl
               TraitType n -> do
                      let args = take n . fmap Id $ identifiersFrom ['a'..'z']
                      return $ Map.insert i (FunctorType args []) modl
  foldM go Map.empty info

unsafeImpl :: Sequence
unsafeImpl = Sequence [Call $ QId [primitivesModuleName, Id "unsafe"]] -- TODO Factor.Names this

bindModule :: (MonadState ReadOnlyState m, MonadError FactorError m) =>
              QId -> QId -> ParameterizedModule -> [QId] -> m RId
bindModule mqid fnqid (ParameterizedModule params info) args
    | length params /= length args = throwError $ FunctorArgError fnqid (length params) (length args)
    | otherwise = do
        zipped <- forM (zip params args) $ \(ModuleArg param (TraitRef req innerargs), arg) -> do
                    modl <- get >>= lookupFn arg >>= \case
                            ModuleValue m -> pure m
                            _ -> throwError (NoSuchModule arg)
                    req' <- get >>= lookupFn req >>= \case
                            TraitValue pt -> get >>= runReaderT (bindTrait req pt innerargs)
                            _ -> throwError (NoSuchTrait req)
                    get >>= runReaderT (moduleSatisfies' req' arg modl)
                    return (param, arg)
        -- TODO Self to Factor.Names
        let submap = Map.fromList (zipped ++ [(Id "Self", mqid)])
            subfn k = maybe (QId [k]) id (Map.lookup k submap)
        modl <- foldM (\m (k, v) -> bindFunctorInfo subfn (mqid <> QId [k]) k v m) emptyModule $ Map.toList info
        appendResourceRO' mqid (ModuleValue modl)

bindFunctorInfo :: (MonadState ReadOnlyState m, MonadError FactorError m) =>
                   (Id -> QId) -> QId -> Id -> FunctorInfo -> Module -> m Module
bindFunctorInfo subfn qid name info m =
    case info of
      FunctorUDFunction ptype (Function v ss) -> do
          let res = UDFunction (subArgInPolyFnType subfn ptype) (Function v $ subArgInSeq subfn ss)
          --res' <- normalizeTypesRes' res
          rid <- appendResourceRO' qid res
          return $ set (moduleNames.at name) (Just rid) m
      FunctorUDMacro ptype (Macro v ss) -> do
          let res = UDMacro (subArgInPolyFnType subfn ptype) (Macro v $ subArgInSeq subfn ss)
          --res' <- normalizeTypesRes' res
          rid <- appendResourceRO' qid res
          return $ set (moduleNames.at name) (Just rid) m
      FunctorModule m1 -> do
                modl <- foldM (\m' (k, v) -> bindFunctorInfo subfn (qid <> QId [k]) k v m') emptyModule $ Map.toList m1
                --modl' <- normalizeTypesRes' (ModuleValue modl)
                rid <- appendResourceRO' qid (ModuleValue modl)
                return $ set (moduleNames.at name) (Just rid) m
      FunctorTrait (ParameterizedTrait args t) -> do
          let -- We don't want to substitute any names bound by the
              -- (parameterized) trait itself.
              subfn' k = if any (\(ModuleArg k' _) -> k == k') args then QId [k] else subfn k
              res = TraitValue (ParameterizedTrait args (substituteTrait subfn' t))
          -- TODO Substitute in args
          --res' <- normalizeTypesRes' res
          rid <- appendResourceRO' qid res
          return $ set (moduleNames.at name) (Just rid) m
      FunctorFunctor args impl -> do
          -- TODO Substitute in args (as above)
          let -- We don't want to substitute any names bound by the
              -- inner functor itself.
              subfn' k = if any (\(ModuleArg k' _) -> k == k') args then QId [k] else subfn k
              impl' = fmap (subArgInFunctorInfo subfn') impl
              res = FunctorValue (ParameterizedModule args impl')
          rid <- appendResourceRO' qid res
          return $ set (moduleNames.at name) (Just rid) m
      FunctorType vs ts -> do
          let ts' = fmap (\(TypeVal t xs) -> TypeVal t $ fmap (subArgInType subfn) xs) ts
          let prefixqid = QId . init $ unQId qid
          overLens readerResources $ declareType (TypeToDeclare prefixqid name vs ts') m

appendResourceRO' :: MonadState ReadOnlyState m => QId -> ReaderValue -> m RId
appendResourceRO' qid value = state (appendResourceRO qid value)

makeFreshModuleName :: String -> ReadOnlyState -> Id
makeFreshModuleName prefix reader = head [name | v <- [0 :: Int ..]
                                               , let name = Id (prefix ++ show v)
                                               , isNothing (reader^.readerNames.at name)]
