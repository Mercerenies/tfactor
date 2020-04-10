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

-- ///// Check for a valid load order (that won't result in weird
-- macro shenanigans) and then load in that order.

-- TODO Once this file is complete, we'll remove the TypeCheckerPass
-- thing fully. For now, this shim will "ignore" the function pass
-- argument.
checkTypeShim :: (MonadError FactorError m, MonadReader ReadOnlyState m) => ReaderValue -> m ()
checkTypeShim r = checkTypeOf FunctionPass r >> checkTypeOf MacroPass r

loadEntity :: (MonadError FactorError m, MonadReader ReadOnlyState m)  => ReaderValue -> m ReaderValue
loadEntity r = augmentWithMacros r >>= \r' -> checkTypeShim r' >> pure r'

loadEntityAt :: MonadError FactorError m => QId -> ReadOnlyState -> m ReadOnlyState
loadEntityAt qid r = traverseOf (atQId qid) (\v -> runReaderT (traverse loadEntity v) r) r

loadEntities :: MonadError FactorError m => [QId] -> ReadOnlyState -> m ReadOnlyState
loadEntities qids r = do
  loadOrder <- determineLoadOrderFor qids r
  foldM (flip loadEntityAt) r loadOrder
