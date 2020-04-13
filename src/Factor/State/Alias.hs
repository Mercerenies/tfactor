{-# LANGUAGE FlexibleContexts, LambdaCase, ViewPatterns #-}

module Factor.State.Alias where

import Factor.State
import Factor.Id
import Factor.Util
import Factor.Error
import Factor.Code

import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.Reader hiding (reader)
import Control.Lens

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
resolveAliasesData m (FunctionValue (Function v seq_)) =
    FunctionValue . Function v <$> resolveAliasesSeq m seq_

resolveAliasesStmt :: MonadError FactorError m => Map Id Alias -> Statement -> m Statement
resolveAliasesStmt m (Call qid) = Call <$> resolveAlias m qid
resolveAliasesStmt m (Literal d) = Literal <$> resolveAliasesData m d

resolveAliasesSeq :: MonadError FactorError m => Map Id Alias -> Sequence -> m Sequence
resolveAliasesSeq m (Sequence xs) = Sequence <$> mapM (resolveAliasesStmt m) xs

-- TODO Aliases in types...
resolveAliasesMod :: (MonadReader ReadOnlyState m, MonadError FactorError m) =>
                     Map Id Alias -> QId -> Module -> m Module
resolveAliasesMod m name modl = do
  let m' = openModule name modl m
  m'' <- foldM handleAliasDecl m' (modl^.moduleAliases)
  let go _ (UDFunction t (Function v ss)) = UDFunction t . Function v <$> resolveAliasesSeq m'' ss
      go _ (bif @ BIFunction {}) = pure bif
      go _ (UDMacro t (Macro v ss)) = UDMacro t . Macro v <$> resolveAliasesSeq m'' ss
      go k (ModuleValue inner) = ModuleValue <$> resolveAliasesMod m'' (name <> QId [k]) inner
  itraverseOf (moduleNames . itraversed) go modl

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
