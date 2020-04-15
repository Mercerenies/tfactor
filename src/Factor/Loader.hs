{-# LANGUAGE FlexibleContexts #-}

module Factor.Loader where

import Factor.State
import Factor.State.Macro
import Factor.Type.Checker
import Factor.Error
import Factor.Id
import Factor.Loader.Graph

import Control.Monad.Except
import Control.Monad.Reader
import Control.Lens

loadEntity :: (MonadError FactorError m, MonadReader ReadOnlyState m)  => ReaderValue -> m ReaderValue
loadEntity r = do
  checkTypeOf MacroPass r
  r' <- augmentWithMacros r
  checkTypeOf FunctionPass r'
  pure r'

loadEntityAt :: MonadError FactorError m => QId -> ReadOnlyState -> m ReadOnlyState
loadEntityAt qid r = traverseOf (atQId qid) (\v -> runReaderT (loadEntity v) r) r

loadEntities :: MonadError FactorError m => [QId] -> ReadOnlyState -> m ReadOnlyState
loadEntities qids r = do
  loadOrder <- determineLoadOrderFor qids r
  foldM (flip loadEntityAt) r loadOrder
