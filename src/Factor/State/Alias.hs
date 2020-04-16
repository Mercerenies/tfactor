{-# LANGUAGE FlexibleContexts, LambdaCase, ViewPatterns #-}

module Factor.State.Alias where

import Factor.State
import Factor.Id
import Factor.Util
import Factor.Error
import Factor.Code
import Factor.Names
import Factor.Type
import qualified Factor.Stack as Stack

import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.List as List
import Control.Monad.Except
import Control.Monad.Reader hiding (reader)
import Control.Lens

-- TODO Currently, we resolve aliases in declaration order. We could
-- make a dependency graph like we do for modules or for macro
-- resolution.

data Alias = AliasValue QId
           | AmbiguousAlias [QId]
             deriving (Show, Eq)

defAlias :: Id -> QId -> Map Id Alias -> Map Id Alias
defAlias v q = insertOrUpdate go v
    where go Nothing = AliasValue q
          go (Just (AliasValue q'))
              | q == q' = AliasValue q
              | otherwise = AmbiguousAlias [q, q']
          go (Just (AmbiguousAlias qs)) = AmbiguousAlias (q:qs)

openModule :: QId -> Module -> Map Id Alias -> Map Id Alias
openModule mname (view moduleNames -> modl) aliases0 = Map.foldlWithKey' go aliases0 modl
    where go aliases k _ = defAlias k (mname <> QId [k]) aliases

lookupAndOpenModule :: MonadError FactorError m =>
                       QId -> ReadOnlyState -> Map Id Alias -> m (Map Id Alias)
lookupAndOpenModule mname reader aliases = lookupFn mname reader >>= \case
                                           ModuleValue m -> return $ openModule mname m aliases
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

resolveAliasesData :: MonadError FactorError m => Map Id Alias -> Data -> m Data
resolveAliasesData _ (Int n) = pure $ Int n
resolveAliasesData _ (Bool b) = pure $ Bool b
resolveAliasesData _ (String s) = pure $ String s
resolveAliasesData _ (Symbol s) = pure $ Symbol s
resolveAliasesData m (RecordInstance i arr) =
    RecordInstance i <$> mapM (resolveAliasesData m) arr
resolveAliasesData m (FunctionValue (Function v seq_)) =
    FunctionValue . Function v <$> resolveAliasesSeq m seq_

resolveAliasesStmt :: MonadError FactorError m => Map Id Alias -> Statement -> m Statement
resolveAliasesStmt m (Call qid) = Call <$> resolveAlias m qid
resolveAliasesStmt m (Literal d) = Literal <$> resolveAliasesData m d

resolveAliasesSeq :: MonadError FactorError m => Map Id Alias -> Sequence -> m Sequence
resolveAliasesSeq m (Sequence xs) = Sequence <$> mapM (resolveAliasesStmt m) xs

resolveAliasesType :: MonadError FactorError m => Map Id Alias -> Type -> m Type
resolveAliasesType _ (PrimType t) = pure $ PrimType t
resolveAliasesType m (FunType fn) = FunType <$> resolveAliasesFnType m fn
resolveAliasesType m (NamedType qid) = NamedType <$> resolveAlias m qid
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
resolveAliasesResource _ (ModuleValue inner) = pure (ModuleValue inner)
resolveAliasesResource m (ModuleSynonym qid) = ModuleSynonym <$> resolveAlias m qid

resolveAliasesResource' :: (MonadError FactorError m, MonadReader ReadOnlyState m) =>
                           Map Id Alias -> QId -> ReaderValue -> m ReaderValue
resolveAliasesResource' aliases0 (QId xs) r = do
  let parents = init $ List.inits xs -- We want all nontrivial prefixes
      handleParentModule aliases mname = do
        m <- case mname of
               [] -> asks (view readerModule)
               mname' -> ask >>= lookupFn (QId mname') >>= \case
                         ModuleValue m -> pure m
                         _ -> throwError (NoSuchModule $ QId mname)
        let aliases' = openModule (QId mname) m aliases
        foldM handleAliasDecl aliases' (m^.moduleAliases)
  aliases1 <- foldM handleParentModule aliases0 parents
  resolveAliasesResource aliases1 r

handleAliasDecl :: (MonadReader ReadOnlyState m, MonadError FactorError m) =>
                   Map Id Alias -> AliasDecl -> m (Map Id Alias)
handleAliasDecl m a = case a of
                        Alias i j -> do
                               j' <- resolveAlias m j
                               -- Ensure that the name exists
                               _ <- ask >>= lookupFn j'
                               pure $ defAlias i j' m
                        Open mname -> do
                               reader <- ask
                               mname' <- resolveAlias m mname
                               lookupAndOpenModule mname' reader m

bindDefaultAliases :: MonadError FactorError m => ReadOnlyState -> Map Id Alias -> m (Map Id Alias)
bindDefaultAliases reader aliases =
    lookupAndOpenModule (QId [preludeModuleName]) reader aliases >>=
    pure . defAlias rootAliasName (QId [])
