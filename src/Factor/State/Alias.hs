{-# LANGUAGE FlexibleContexts #-}

module Factor.State.Alias where

import Factor.State
import Factor.Id
import Factor.Util
import Factor.Error
import Factor.Code

import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad.Except

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

openModule :: QId -> Map Id ReaderValue -> Map Id Alias -> Map Id Alias
openModule mname modl aliases0 = Map.foldlWithKey' go aliases0 modl
    where go aliases k _ = defAlias k (mname <> QId [k]) aliases

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
resolveAliasesData m (FunctionValue (Function v seq_)) =
    FunctionValue . Function v <$> resolveAliasesSeq m seq_

resolveAliasesStmt :: MonadError FactorError m => Map Id Alias -> Statement -> m Statement
resolveAliasesStmt m (Call qid) = Call <$> resolveAlias m qid
resolveAliasesStmt m (Literal d) = Literal <$> resolveAliasesData m d

resolveAliasesSeq :: MonadError FactorError m => Map Id Alias -> Sequence -> m Sequence
resolveAliasesSeq m (Sequence xs) = Sequence <$> mapM (resolveAliasesStmt m) xs