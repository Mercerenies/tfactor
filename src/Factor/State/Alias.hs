{-# LANGUAGE FlexibleContexts #-}

module Factor.State.Alias where

import Factor.State
import Factor.Id
import Factor.Util
import Factor.Error

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
