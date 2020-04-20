{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Factor.Trait.Functor(makeMinimalModule, bindModule, makeFreshModuleName) where

import Factor.Trait
import Factor.Trait.Types
import Factor.Trait.Argument
import Factor.State.Reader
import Factor.State.Resource
import Factor.Error
import Factor.Id
import Factor.Util
import Factor.Names
import Factor.Code

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader hiding (reader)
import Control.Lens
import qualified Data.Map as Map
import Data.Maybe

-- Makes a minimal module for a functor, used for type-checking.
makeMinimalModule :: (MonadState ReadOnlyState m, MonadError FactorError m) =>
                     QId -> ParameterizedModule -> m (Module, RId)
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
                  Just (ModuleValue m) -> return (m, rid)
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
               TraitDemandType -> return $ set moduleIsType True modl
  dat <- foldM go emptyModule info
  rid <- appendResourceRO' qid (ModuleValue dat)
  return (dat, rid)

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
                    get >>= runReaderT (moduleSatisfies' req' modl)
                    return (param, arg)
        let submap = Map.fromList zipped
            subfn k = maybe (QId [k]) id (Map.lookup k submap)
        modl <- Map.traverseWithKey (\k v -> bindFunctorInfo subfn (mqid <> QId [k]) v) info
        appendResourceRO' mqid (ModuleValue $ mapToModule modl)

bindFunctorInfo :: (MonadState ReadOnlyState m, MonadError FactorError m) =>
                   (Id -> QId) -> QId -> FunctorInfo -> m RId
bindFunctorInfo subfn qid info =
    case info of
      -- TODO Substitute in the function sequences too
      FunctorUDFunction ptype fn -> appendResourceRO' qid (UDFunction (subArgInPolyFnType subfn ptype) fn)
      FunctorUDMacro ptype fn -> appendResourceRO' qid (UDMacro (subArgInPolyFnType subfn ptype) fn)
      FunctorModule m -> do
                modl <- Map.traverseWithKey (\k v -> bindFunctorInfo subfn (qid <> QId [k]) v) m
                appendResourceRO' qid (ModuleValue $ mapToModule modl)
      FunctorTrait (ParameterizedTrait args t) ->
          let -- We don't want to substitute any names bound by the
              -- (parameterized) trait itself.
              subfn' k = if any (\(ModuleArg k' _) -> k == k') args then QId [k] else subfn k
          in -- TODO Substitute in args
             appendResourceRO' qid (TraitValue (ParameterizedTrait args (substituteTrait subfn' t)))

appendResourceRO' :: MonadState ReadOnlyState m => QId -> ReaderValue -> m RId
appendResourceRO' qid value = state (appendResourceRO qid value)

makeFreshModuleName :: String -> ReadOnlyState -> Id
makeFreshModuleName prefix reader = head [name | v <- [0 :: Int ..]
                                               , let name = Id (prefix ++ show v)
                                               , isNothing (reader^.readerNames.at name)]
