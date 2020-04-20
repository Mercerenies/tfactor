{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Factor.Trait.Functor where

import Factor.Trait
import Factor.Trait.Types
import Factor.State.Reader
import Factor.State.Resource
import Factor.Error
import Factor.Id

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader

bindModule :: (MonadState ReadOnlyState m, MonadError FactorError m) =>
              QId -> ParameterizedModule -> [QId] -> m RId
bindModule qid (ParameterizedModule params _info) args
    | length params /= length args = throwError $ FunctorArgError qid (length params) (length args)
    | otherwise = do
        _zipped <- forM (zip params args) $ \(ModuleArg param (TraitRef req innerargs), arg) -> do
                    modl <- get >>= lookupFn arg >>= \case
                            ModuleValue m -> pure m
                            _ -> throwError (NoSuchModule arg)
                    req' <- get >>= lookupFn req >>= \case
                            TraitValue pt -> get >>= runReaderT (bindTrait req pt innerargs)
                            _ -> throwError (NoSuchTrait req)
                    get >>= runReaderT (moduleSatisfies' req' modl)
                    return (param, arg)
        undefined
