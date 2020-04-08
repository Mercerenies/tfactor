{-# LANGUAGE FlexibleContexts #-}

module Factor.Loader where

import Factor.State
import Factor.State.Macro
import Factor.Type.Checker
import Factor.Error
--import Factor.Id

import Control.Monad.Except
import Control.Monad.Reader

-- ///// Check for a valid load order (that won't result in weird
-- macro shenanigans) and then load in that order.

-- TODO Once this file is complete, we'll remove the TypeCheckerPass thing fully

loadEntity :: (MonadError FactorError m, MonadReader ReadOnlyState m)  => ReaderValue -> m ReaderValue
loadEntity r = augmentWithMacros r >>= \r' -> checkTypeOf FunctionPass r' >> pure r'
