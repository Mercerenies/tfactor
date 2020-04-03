{-# LANGUAGE FlexibleContexts #-}

module Factor.Type.Checker where

import Factor.Code
import Factor.State
import Factor.Type
import Factor.Type.Unify
import Factor.Error

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except

typeOfValue :: (MonadError FactorError m, MonadReader ReadOnlyState m,
                MonadWriter AssumptionsAll m) =>
               Data -> m Type
typeOfValue value = case value of
                      Int _ -> return (PrimType TInt)
                      FunctionValue (Function _ ss) -> (FunType . underlyingFnType) <$> typeOfSeq ss

-- A single statement always carries an effect on the stack, hence we
-- treat its type as a function type.
typeOf :: (MonadError FactorError m, MonadReader ReadOnlyState m, MonadWriter AssumptionsAll m) =>
          Statement -> m PolyFunctionType
typeOf stmt = case stmt of
                Call v -> ask >>= lookupFn' v >>= return . readerFunctionType
                Literal d -> (\t -> polyFunctionType [] [] [t]) <$> typeOfValue d

typeOfSeq :: (MonadError FactorError m, MonadReader ReadOnlyState m, MonadWriter AssumptionsAll m) =>
             Sequence -> m PolyFunctionType
typeOfSeq (Sequence xs) = mapM typeOf xs >>= foldM composePFunctions emptyPolyFnType

-- When checking the declared type of a thing, all we care about is
-- that the types line up. The assumptions we used to get there are
-- irrelevant.
checkDeclaredType :: (MonadError FactorError m, MonadReader ReadOnlyState m) => PolyFunctionType -> Function -> m ()
checkDeclaredType (PolyFunctionType ids declared) (Function _ ss) = fmap fst . runWriterT $ do
  ss' <- typeOfSeq ss
  let inferred = underlyingFnType ss'
  toGround' ids inferred `isFnSubtypeOf` toGround' ids declared
      where toGround' i fn = case toGround i (FunType fn) of
                               FunType fn' -> fn'
                               _ -> error "toGround changed shape of type in checkDeclaredType"
