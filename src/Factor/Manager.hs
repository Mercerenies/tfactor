{-# LANGUAGE FlexibleContexts #-}

module Factor.Manager where

import Factor.Error
import Factor.State
import Factor.State.Alias
import Factor.State.Reader
import Factor.State.Resource
import Factor.Loader
import Factor.Loader.Module
import Factor.Loader.Type
import Factor.Id

import Control.Monad.Except
import Control.Monad.Reader hiding (reader)
import Control.Lens
import qualified Data.Map as Map

fullyLoadBindings :: MonadError FactorError m =>
                     (ReadOnlyState -> m ReadOnlyState) -> ReadOnlyState -> m ReadOnlyState
fullyLoadBindings bindfn newbindings = do
  fullbindings <- bindfn newbindings
  aliases <- bindDefaultAliases fullbindings Map.empty
  newbindings' <- runReaderT (forOf (readerResources.traverseWithQId) newbindings $ \(q, v) -> resolveAliasesResource' aliases q v) fullbindings
  reader <- bindfn newbindings'
  let names = allNames newbindings'
  reader' <- loadModules names reader
  reader'' <- runReaderT (forOf (readerResources.traverseWithQId) reader' $ \(q, v) -> resolveAliasesResource' aliases q v) reader'
  let names' = concatMap (allChildrenOf reader'') names
  reader''' <- normalizeAllTypes names' reader''
  loadEntities names' reader'''

loadBindings :: MonadError FactorError m =>
                [QId] -> ReadOnlyState -> m ReadOnlyState
loadBindings names = normalizeAllTypes names >=> loadEntities names
