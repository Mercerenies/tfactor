{-# LANGUAGE FlexibleContexts, LambdaCase, ViewPatterns #-}

module Factor.State.Alias where

import Factor.State.Reader
import Factor.Id
import Factor.Util
import Factor.Error
import Factor.Code
import Factor.Names
import Factor.Type
import qualified Factor.Stack as Stack
import Factor.Trait
import Factor.Trait.Types
import Factor.Loader.Type

import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Set(Set)
import qualified Data.Set as Set
import Control.Monad.Except
import Control.Monad.Reader hiding (reader)
import Control.Lens

-- TODO Currently, we resolve aliases in declaration order. We could
-- make a dependency graph like we do for modules or for macro
-- resolution.

data Alias = AliasValue QId
           | AmbiguousAlias [QId]
             deriving (Show, Eq)

data NameExist = NoExist
               | NamedResource ReaderValue
               | NamedTraitEntity TraitInfo
                 deriving (Show)

-- NameExist is a relatively trivial monoid, whose identity is NoExist
-- and for which, other than the identity, the <> operation always
-- returns the left operand.
instance Semigroup NameExist where
    NoExist <> y = y
    x <> _ = x

instance Monoid NameExist where
    mempty = NoExist

data IntermediateLookup = ILIsMod Module
                        | ILIsTrait Trait
                          deriving (Show)

-- TODO Will need to implement includeNameInModule for the inside of
-- functors once includes are allowed inside functors.

-- This isn't exactly like Factor.State.Reader.lookupFn. That function
-- assumes the ID points to a valid resource, whereas this one is
-- willing to refer to hypothetical resources that haven't been
-- allocated yet, since during alias resolution we don't care about a
-- thing's resource ID; we simply want to know that it either exists
-- or will exist soon.
doesNameExist :: Map Id Alias -> ReadOnlyState -> QId -> NameExist
doesNameExist m r q = doesNameExist0 m r Set.empty q

-- doesNameExist calls this function, which uses a set to execute
-- recursion safeguards. Don't call this function directly.
doesNameExist0 :: Map Id Alias -> ReadOnlyState -> Set QId -> QId -> NameExist
doesNameExist0 _ _ s q | q `Set.member` s = NoExist -- TODO Maybe this should be an error?
doesNameExist0 m0 r s (QId xs0) = go (ILIsMod (r^.readerModule)) xs0
    where s' = Set.insert (QId xs0) s
          conclude :: IntermediateLookup -> NameExist
          conclude (ILIsMod m) = NamedResource $ ModuleValue m
          conclude (ILIsTrait t) = NamedResource $ TraitValue (ParameterizedTrait [] t)
          go :: IntermediateLookup -> [Id] -> NameExist
          go intl [] = conclude intl
          go (ILIsMod m) [x] = nameInModule0 m0 r m s' x
          go (ILIsTrait t) [x] =
              case traitAt r t x of
                Left _ -> NoExist
                Right y -> NamedTraitEntity y
          go (ILIsMod m) (x:xs) =
              case nameInModule0 m0 r m s' x of
                NoExist -> NoExist
                NamedResource (ModuleValue m') -> go (ILIsMod m') xs
                NamedResource (SynonymPlaceholder (SynonymGeneral q)) ->
                    doesNameExist0 m0 r s' (resolveAliasIgnoreAmbiguity m0 $ q <> QId xs) -- TODO Ambiguity error?
                NamedResource (SynonymPlaceholder (ActualizeFunctor (TraitRef q _))) ->
                    case doesNameExist0 m0 r s' (resolveAliasIgnoreAmbiguity m0 q) of -- TODO Ambiguity error?
                      NoExist -> NoExist
                      NamedResource (FunctorValue pm) ->
                          let ParameterizedTrait _ t = functorToTrait q pm
                          in go (ILIsTrait t) xs
                      NamedResource _ -> NoExist
                      NamedTraitEntity (TraitFunctor _ ys) -> go (ILIsTrait (Trait ys)) xs
                      NamedTraitEntity _ -> NoExist
                NamedResource _ -> NoExist
                NamedTraitEntity (TraitModule ys) -> go (ILIsTrait (Trait ys)) xs
                NamedTraitEntity _ -> NoExist
          go (ILIsTrait t) (x:xs) =
              case traitAt r t x of
                Left _ -> NoExist
                Right (TraitModule ys) -> go (ILIsTrait (Trait ys)) xs
                Right _ -> NoExist

nameInModule :: Map Id Alias -> ReadOnlyState -> Module -> Id -> NameExist
nameInModule m0 r m x = nameInModule0 m0 r m Set.empty x

nameInModule0 :: Map Id Alias -> ReadOnlyState -> Module -> Set QId -> Id -> NameExist
nameInModule0 m0 r m s x = stdNameInModule <> includeNameInModule
    where stdNameInModule = case Map.lookup x (m^.moduleNames) >>= \rid -> r^.readerResources.possibly (ix rid) of
                              Nothing -> NoExist
                              Just y -> NamedResource y
          includeNameInModule = foldMap includeNameInModule' (m^.moduleDecls)
          includeNameInModule' (IncludeModule qid)
              | qid `Set.member` s = NoExist -- TODO Cycle error?
              | otherwise = doesNameExist0 m0 r s (qid <> QId [x])
          includeNameInModule' _ = NoExist

defAlias :: Id -> QId -> Map Id Alias -> Map Id Alias
defAlias v q = insertOrUpdate go v
    where go Nothing = AliasValue q
          go (Just (AliasValue q'))
              | q == q' = AliasValue q
              | otherwise = AmbiguousAlias [q, q']
          go (Just (AmbiguousAlias qs)) = AmbiguousAlias (q:qs)

openModule :: ReadOnlyState -> QId -> Module -> Map Id Alias -> Map Id Alias
openModule r mname modl aliases0 = openModule0 r mname modl Set.empty aliases0

openModule0 :: ReadOnlyState -> QId -> Module -> Set QId -> Map Id Alias -> Map Id Alias
openModule0 r mname modl s aliases0 = aliases2
    where aliases1 = Map.foldlWithKey' go aliases0 (modl^.moduleNames)
          aliases2 = List.foldl' go' aliases1 (modl^.moduleDecls)
          go aliases k _ = defAlias k (mname <> QId [k]) aliases
          go' aliases (IncludeModule q) =
             case doesNameExist0 aliases r s q of
               NoExist -> aliases
               NamedResource (ModuleValue m) -> openModule0 r mname m (Set.insert q s) aliases
               NamedResource _ -> aliases
               NamedTraitEntity (TraitModule _m) -> aliases -- ////
               NamedTraitEntity _ -> aliases
          go' aliases _ = aliases

lookupAndOpenModule :: MonadError FactorError m =>
                       QId -> ReadOnlyState -> Map Id Alias -> m (Map Id Alias)
lookupAndOpenModule mname reader aliases = lookupFn mname reader >>= \case
                                           ModuleValue m -> return $ openModule reader mname m aliases
                                           _ -> throwError (NoSuchModule mname)

-- Looking up an alias that doesn't exist is not an error; it simply
-- means we're not using an alias. Looking up an ambiguous alias is a
-- compile error.
lookupAlias :: MonadError FactorError m => Id -> Map Id Alias -> m QId
lookupAlias v m = case Map.lookup v m of
                    Nothing -> pure $ QId [v]
                    Just (AliasValue x) -> pure x
                    Just (AmbiguousAlias xs) -> throwError (AmbiguousName v xs)

resolveAlias :: MonadError FactorError m => Map Id Alias -> QId -> m QId
resolveAlias _ (QId []) = pure (QId [])
resolveAlias m (QId (x:xs)) =
    -- Lookup the first component of a qualified identifier.
    lookupAlias x m >>= \x' -> pure (x' <> QId xs)

resolveAliasIgnoreAmbiguity :: Map Id Alias -> QId -> QId
resolveAliasIgnoreAmbiguity m q = case resolveAlias m q of
                                    Left _ -> q
                                    Right q' -> q'

resolveAliasesData :: MonadError FactorError m => Map Id Alias -> Data -> m Data
resolveAliasesData _ (Int n) = pure $ Int n
resolveAliasesData _ (Bool b) = pure $ Bool b
resolveAliasesData _ (String s) = pure $ String s
resolveAliasesData _ (Symbol s) = pure $ Symbol s
resolveAliasesData m (RecordInstance i n arr) =
    RecordInstance i n <$> mapM (resolveAliasesData m) arr
resolveAliasesData m (FunctionValue (Function v seq_)) =
    FunctionValue . Function v <$> resolveAliasesSeq m seq_

resolveAliasesStmt :: MonadError FactorError m => Map Id Alias -> Statement -> m Statement
resolveAliasesStmt m (Call qid) = Call <$> resolveAlias m qid
resolveAliasesStmt m (Literal d) = Literal <$> resolveAliasesData m d

resolveAliasesSeq :: MonadError FactorError m => Map Id Alias -> Sequence -> m Sequence
resolveAliasesSeq m (Sequence xs) = Sequence <$> mapM (resolveAliasesStmt m) xs

resolveAliasesType :: MonadError FactorError m => Map Id Alias -> Type -> m Type
resolveAliasesType m (FunType fn) = FunType <$> resolveAliasesFnType m fn
resolveAliasesType m (NamedType (TypeId qid ts)) =
    (\a b -> NamedType (TypeId a b)) <$> resolveAlias m qid <*> mapM (resolveAliasesType m) ts
resolveAliasesType _ (GroundVar i) = pure $ GroundVar i
resolveAliasesType _ (QuantVar i) = pure $ QuantVar i

resolveAliasesFnType :: MonadError FactorError m => Map Id Alias -> FunctionType -> m FunctionType
resolveAliasesFnType m (FunctionType (StackDesc args a) (StackDesc rets r)) = do
  Stack.FromTop args' <- mapM (resolveAliasesType m) (Stack.FromTop args)
  Stack.FromTop rets' <- mapM (resolveAliasesType m) (Stack.FromTop rets)
  return $ FunctionType (StackDesc args' a) (StackDesc rets' r)

resolveAliasesPolyFnType :: MonadError FactorError m =>
                            Map Id Alias -> PolyFunctionType -> m PolyFunctionType
resolveAliasesPolyFnType m (PolyFunctionType i f) =
    PolyFunctionType i <$> resolveAliasesFnType m f

resolveAliasesTrait :: MonadError FactorError m =>
                       Map Id Alias -> TraitInfo -> m TraitInfo
resolveAliasesTrait m (TraitFunction p) = TraitFunction <$> resolveAliasesPolyFnType m p
resolveAliasesTrait m (TraitMacro p) = TraitMacro <$> resolveAliasesPolyFnType m p
resolveAliasesTrait m (TraitModule xs) =
    TraitModule <$> mapM (\(i, t) -> ((,) i) <$> resolveAliasesTrait m t) xs
resolveAliasesTrait m (TraitInclude (TraitRef qid args)) = do
  qid' <- resolveAlias m qid
  args' <- mapM (resolveAlias m) args
  return (TraitInclude (TraitRef qid' args'))
--resolveAliasesTrait _ TraitDemandType = pure TraitDemandType
resolveAliasesTrait m (TraitFunctor args xs) = do
  args' <- forM args $ \(ModuleArg name (TraitRef tname innerargs)) -> do
                           tname' <- resolveAlias m tname
                           innerargs' <- mapM (resolveAlias m) innerargs
                           return $ ModuleArg name (TraitRef tname' innerargs')
  -- TODO Again, do we care about shadowing here? (See comments below in this file)
  xs' <- forM xs $ \(i, t) -> (,) i <$> resolveAliasesTrait m t
  return (TraitFunctor args' xs')
resolveAliasesTrait _ (TraitType n) = pure $ TraitType n

resolveAliasesAssert :: MonadError FactorError m =>
                        Map Id Alias -> ModuleDecl -> m ModuleDecl
resolveAliasesAssert m (AssertTrait (TraitRef qid args)) = do
  qid' <- resolveAlias m qid
  args' <- mapM (resolveAlias m) args
  return (AssertTrait (TraitRef qid' args'))
resolveAliasesAssert m (IncludeModule q) = IncludeModule <$> resolveAlias m q
-- Note that we do nothing with these two, since they've already been
-- resolved by this point, so it honestly doesn't matter that much.
resolveAliasesAssert _ (Open q) = pure (Open q)
resolveAliasesAssert _ (Alias i q) = pure (Alias i q)

resolveAliasesFunctorInfo :: MonadError FactorError m =>
                             QId -> Map Id Alias -> FunctorInfo -> m FunctorInfo
resolveAliasesFunctorInfo _ m (FunctorUDFunction t (Function v ss)) = do
  ss' <- resolveAliasesSeq m ss
  t' <- resolveAliasesPolyFnType m t
  return $ FunctorUDFunction t' (Function v ss')
resolveAliasesFunctorInfo _ m (FunctorUDMacro t (Macro v ss)) = do
  ss' <- resolveAliasesSeq m ss
  t' <- resolveAliasesPolyFnType m t
  return $ FunctorUDMacro t' (Macro v ss')
resolveAliasesFunctorInfo q m (FunctorModule inner) =
    let m' = bindAliasesForFunctor q inner m
    in FunctorModule <$> Map.traverseWithKey (resolveAliasesFunctorInfo' q m') inner
resolveAliasesFunctorInfo _ m (FunctorTrait (ParameterizedTrait params (Trait xs))) = do
  xs' <- mapM (\(i, t) -> ((,) i) <$> resolveAliasesTrait m t) xs
  -- TODO Shadowing issues with the names bound by the trait? Or do we care?
  params' <- forM params $ \(ModuleArg i (TraitRef name args)) -> do
               name' <- resolveAlias m name
               args' <- mapM (resolveAlias m) args
               return (ModuleArg i (TraitRef name' args'))
  return (FunctorTrait (ParameterizedTrait params' (Trait xs')))
resolveAliasesFunctorInfo q m (FunctorFunctor args xs) = do
  -- TODO The name ends up wrong here, since Self will now refer to
  -- the inner functor, not the outer. Or it might not. Honestly, Self
  -- with nested functors is pretty buggy right now anyway.
  args' <- forM args $ \(ModuleArg name (TraitRef tname innerargs)) -> do
                           tname' <- resolveAlias m tname
                           innerargs' <- mapM (resolveAlias m) innerargs
                           return $ ModuleArg name (TraitRef tname' innerargs')
  -- TODO Again, do we care about shadowing here? (See comments below in this file)
  xs' <- Map.traverseWithKey (resolveAliasesFunctorInfo' q m) xs
  return (FunctorFunctor args' xs')
resolveAliasesFunctorInfo _ m (FunctorType vs ts) = do
  ts' <- forM ts $ \(TypeVal t xs) ->
           TypeVal t <$> _Unwrapping Stack.FromTop (mapM (resolveAliasesType m)) xs
  return $ FunctorType vs ts'
resolveAliasesFunctorInfo _ _ FunctorGenerated = pure FunctorGenerated

resolveAliasesFunctorInfo' :: MonadError FactorError m =>
                              QId -> Map Id Alias -> Id -> FunctorInfo -> m FunctorInfo
resolveAliasesFunctorInfo' q m i f = resolveAliasesFunctorInfo (q <> QId [i]) m f

resolveAliasesResource :: MonadError FactorError m =>
                          Map Id Alias -> ReaderValue -> m ReaderValue
resolveAliasesResource m (UDFunction t (Function v ss)) = do
  ss' <- resolveAliasesSeq m ss
  t' <- resolveAliasesPolyFnType m t
  return $ UDFunction t' (Function v ss')
resolveAliasesResource _ (bif @ BIFunction {}) = pure bif
resolveAliasesResource m (UDMacro t (Macro v ss)) = do
  ss' <- resolveAliasesSeq m ss
  t' <- resolveAliasesPolyFnType m t
  return $ UDMacro t' (Macro v ss')
resolveAliasesResource m (ModuleValue inner) =
    ModuleValue <$> traverseOf (moduleDecls.traverse) (resolveAliasesAssert m) inner
resolveAliasesResource m (TraitValue (ParameterizedTrait params (Trait xs))) = do
  xs' <- mapM (\(i, t) -> ((,) i) <$> resolveAliasesTrait m t) xs
  -- TODO Shadowing issues with the names bound by the trait? Or do we care?
  params' <- forM params $ \(ModuleArg i (TraitRef name args)) -> do
               name' <- resolveAlias m name
               args' <- mapM (resolveAlias m) args
               return (ModuleArg i (TraitRef name' args'))
  return (TraitValue (ParameterizedTrait params' (Trait xs')))
resolveAliasesResource m (FunctorValue (ParameterizedModule params info)) = do
  let self = QId [Id "Self"] -- TODO Factor.Names
      m' = bindAliasesForFunctor self info m
  info' <- Map.traverseWithKey (resolveAliasesFunctorInfo' self m') info
  -- TODO Shadowing issues with the names bound by the functor? Or do we care?
  params' <- forM params $ \(ModuleArg i (TraitRef name args)) -> do
               name' <- resolveAlias m name
               args' <- mapM (resolveAlias m) args
               return (ModuleArg i (TraitRef name' args'))
  return (FunctorValue (ParameterizedModule params' info'))
resolveAliasesResource _ (TypeValue (TypeData n)) = pure (TypeValue (TypeData n))
resolveAliasesResource m (SynonymPlaceholder t) = do
  t' <- case t of
          SynonymGeneral q -> SynonymGeneral <$> resolveAlias m q
          ActualizeFunctor (TraitRef name args) -> do
                 name' <- resolveAlias m name
                 args' <- mapM (resolveAlias m) args
                 return (ActualizeFunctor (TraitRef name' args'))
  return (SynonymPlaceholder t')

resolveAliasesResource' :: (MonadError FactorError m, MonadReader ReadOnlyState m) =>
                           Map Id Alias -> QId -> ReaderValue -> m ReaderValue
resolveAliasesResource' aliases0 (QId xs) r = do
  let parents = List.inits xs
      handleParentModule aliases mname = do
        m <- case mname of
               [] -> Just <$> asks (view readerModule)
               mname' -> ask >>= lookupFn (QId mname') >>= \case
                         ModuleValue m -> pure (Just m)
                         _ -> pure Nothing
        case m of
          Nothing -> pure aliases
          Just m' -> do
                 reader <- ask
                 let aliases' = openModule reader (QId mname) m' aliases
                 foldM handleAliasDecl aliases' (m'^.moduleDecls)
  aliases1 <- foldM handleParentModule aliases0 parents
  resolveAliasesResource aliases1 r

bindAliasesForFunctor :: QId -> Map Id FunctorInfo -> Map Id Alias -> Map Id Alias
bindAliasesForFunctor qid mf m0 = foldl go m0 $ Map.toList mf
    where go m (i, _) = defAlias i (qid <> QId [i]) m

handleAliasDecl :: (MonadReader ReadOnlyState m, MonadError FactorError m) =>
                   Map Id Alias -> ModuleDecl -> m (Map Id Alias)
handleAliasDecl m a = case a of
                        Alias i j -> do
                               j' <- resolveAlias m j
                               -- Ensure that the name exists
                               result <- ask >>= \r -> pure (doesNameExist m r j')
                               case result of
                                 NoExist -> throwError (NoSuchFunction j') -- TODO Error messages...
                                 _ -> pure ()
                               pure $ defAlias i j' m
                        Open mname -> do
                               reader <- ask
                               mname' <- resolveAlias m mname
                               lookupAndOpenModule mname' reader m -- //// Update for doesNameExist above
                        AssertTrait _ -> pure m
                        IncludeModule _ -> pure m

bindDefaultAliases :: MonadError FactorError m => ReadOnlyState -> Map Id Alias -> m (Map Id Alias)
bindDefaultAliases reader aliases =
    lookupAndOpenModule (QId [preludeModuleName]) reader aliases >>=
    pure . defAlias rootAliasName (QId [])
