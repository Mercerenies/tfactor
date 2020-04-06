{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}

module Factor.Type.Checker where

import Factor.Code
import Factor.State
import Factor.Type
import Factor.Type.Unify
import Factor.Error
import Factor.Id

import Data.Set(Set)
import qualified Data.Set as Set
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

monomorphize :: MonadState (Set Id) m => PolyFunctionType -> m FunctionType
monomorphize (PolyFunctionType ids fn) = do
  knowns <- get
  let isConflicting v = v `elem` ids && v `elem` knowns
  return $ renameToAvoidConflicts' isConflicting fn

monomorphize' :: Set Id -> PolyFunctionType -> FunctionType
monomorphize' v p = evalState (monomorphize p) v

knownVars :: MonadState (Set Id) m => [Id] -> m ()
knownVars vs = modify $ (Set.fromList vs `Set.union`)

typeOfValue :: (MonadError FactorError m, MonadReader ReadOnlyState m,
                MonadWriter AssumptionsAll m) =>
               Data -> StateT (Set Id) m Type
typeOfValue value = case value of
                      Int _ -> return (PrimType TInt)
                      FunctionValue (Function _ ss) -> FunType <$> (typeOfSeq ss >>= monomorphize)
                      Bool _ -> return (PrimType TBool)

-- A single statement always carries an effect on the stack, hence we
-- treat its type as a function type.
typeOf :: (MonadError FactorError m, MonadReader ReadOnlyState m, MonadWriter AssumptionsAll m) =>
          Statement -> StateT (Set Id) m PolyFunctionType
typeOf stmt = case stmt of
                Call v -> ask >>= lookupFn' v >>= return . readerFunctionType
                Literal d -> (\t -> polyFunctionType [Id "R"] [] (RestQuant $ Id "R") [t] (RestQuant $ Id "R")) <$> typeOfValue d

typeOfSeq :: (MonadError FactorError m, MonadReader ReadOnlyState m, MonadWriter AssumptionsAll m) =>
             Sequence -> StateT (Set Id) m PolyFunctionType
typeOfSeq (Sequence xs) = foldM go emptyPolyFnType xs
    where go acc z = do
            knownVars $ quantifiedVars acc
            z' <- typeOf z
            knownVars $ quantifiedVars z'
            res <- composePFunctions acc z'
            knownVars $ quantifiedVars res
            return res

-- When checking the declared type of a thing, all we care about is
-- that the types line up. The assumptions we used to get there are
-- irrelevant.
checkDeclaredType :: (MonadError FactorError m, MonadReader ReadOnlyState m) =>
                     PolyFunctionType -> Function -> m ()
checkDeclaredType (PolyFunctionType ids declared) (Function _ ss) = runWriterT go >>= doSub
    where go = do
            ss' <- evalStateT (typeOfSeq ss) mempty
            let inferred = underlyingFnType ss'
            inferred `isFnSubtypeOf` toGround' ids declared
          toGround' i fn = case toGround i (FunType fn) of
                             FunType fn' -> fn'
                             _ -> error "toGround changed shape of type in checkDeclaredType"
          doSub ((), w) = do
                 Assumptions _ _ <- consolidateUntilDone w
                 pure ()
