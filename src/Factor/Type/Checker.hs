{-# LANGUAGE FlexibleContexts #-}

module Factor.Type.Checker where

import Factor.Code
import Factor.State
import Factor.Type
import Factor.Type.Unify
import Factor.Error
import qualified Factor.Stack as Stack

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except

typeOfValue :: (MonadError FactorError m, MonadReader ReadOnlyState m,
                MonadWriter AssumptionsAll m) =>
               Data -> m Type
typeOfValue value = case value of
                      Int _ -> return (PrimType TInt)
                      FunctionValue (Function _ ss) -> FunType <$> typeOfSeq ss

-- A single statement always carries an effect on the stack, hence we
-- treat its type as a function type.
typeOf :: (MonadError FactorError m, MonadReader ReadOnlyState m, MonadWriter AssumptionsAll m) =>
          Statement -> m FunctionType
typeOf stmt = case stmt of
                Call v -> ask >>= lookupFn' v >>= return . readerFunctionType
                Literal d -> (\t -> FunctionType Stack.empty (Stack.singleton t)) <$> typeOfValue d

typeOfSeq :: (MonadError FactorError m, MonadReader ReadOnlyState m, MonadWriter AssumptionsAll m) =>
             Sequence -> m FunctionType
typeOfSeq (Sequence xs) = mapM typeOf xs >>= foldM composeFunctions emptyFnType

-- When checking the declared type of a thing, all we care about is
-- that the types line up. The assumptions we used to get there are
-- irrelevant.
checkDeclaredType :: (MonadError FactorError m, MonadReader ReadOnlyState m) => FunctionType -> Function -> m ()
checkDeclaredType declared (Function _ ss) =
    let check = typeOfSeq ss >>= (`isFnSubtypeOf` declared)
    in fmap fst $ runWriterT check
