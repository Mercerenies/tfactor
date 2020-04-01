{-# LANGUAGE LambdaCase #-}

module Factor.Eval where

import Factor.Code
import Factor.Error
import Factor.State
import Factor.Id

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

evalStmt :: Monad m => Statement -> EvalT m ()
evalStmt (Literal d) = pushStack [d]
evalStmt (Call v) = callFunction v

evalSeq :: Monad m => Sequence -> EvalT m ()
evalSeq (Sequence xs) = mapM_ evalStmt xs

callFunction :: Monad m => Id -> EvalT m ()
callFunction v = asks (lookupFn v) >>= \case
                 Nothing -> throwError (NoSuchFunction v)
                 Just (Function _ ss) -> evalSeq ss
