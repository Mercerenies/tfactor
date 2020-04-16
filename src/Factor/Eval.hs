{-# LANGUAGE LambdaCase, FlexibleContexts #-}

module Factor.Eval(EvalT, Eval,
                   runEvalT, evalEvalT, runEval, evalEval,
                   evalStmt, evalSeq,
                   callFunction) where

import Factor.Code
import Factor.Error
import Factor.State
import Factor.State.Stack
import Factor.State.Types(BuiltIn(..))
import Factor.Id
import qualified Factor.Stack as Stack

import Control.Monad.RWS
import Control.Monad.Except
import Data.Functor.Identity

type EvalT m = RWST ReadOnlyState () EvalState (ExceptT FactorError m)

type Eval = EvalT Identity

runEvalT :: Monad m => EvalT m a -> ReadOnlyState -> EvalState ->
            m (Either FactorError (a, EvalState))
runEvalT val r0 s0 = runExceptT . fmap (\(a, b, ()) -> (a, b)) $ runRWST val r0 s0

-- Lovely function name, I know.
evalEvalT :: Monad m => EvalT m a -> ReadOnlyState -> EvalState -> m (Either FactorError a)
evalEvalT val r0 s0 = fmap (fmap fst) $ runEvalT val r0 s0

runEval :: Eval a -> ReadOnlyState -> EvalState -> Either FactorError (a, EvalState)
runEval val r0 s0 = runIdentity $ runEvalT val r0 s0

evalEval :: Eval a -> ReadOnlyState -> EvalState -> Either FactorError a
evalEval val r0 s0 = fmap fst $ runEval val r0 s0

evalStmt :: (MonadReader ReadOnlyState m, MonadState EvalState m, MonadError FactorError m) =>
            Statement -> m ()
evalStmt (Literal d) = pushStack (Stack.singleton d)
evalStmt (Call v) = callFunction v

evalSeq :: (MonadReader ReadOnlyState m, MonadState EvalState m, MonadError FactorError m) =>
           Sequence -> m ()
evalSeq (Sequence xs) = mapM_ evalStmt xs

callFunction :: (MonadReader ReadOnlyState m, MonadState EvalState m, MonadError FactorError m) =>
                QId -> m ()
callFunction v = ask >>= lookupFn v >>= go
    where go (UDFunction _ (Function _ ss)) = evalSeq ss
          go (BIFunction _ (BuiltIn f)) = f
          go (UDMacro _ _) = throwError NotAFunction
          go (ModuleValue _) = throwError NotAFunction
          go (ModuleSynonym _) = throwError NotAFunction
