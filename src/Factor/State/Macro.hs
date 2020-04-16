{-# LANGUAGE FlexibleContexts #-}

module Factor.State.Macro where

import Factor.State
import Factor.State.Stack
import Factor.Eval
import Factor.Error
import Factor.Code
import qualified Factor.Stack as Stack
import Factor.Id
import Factor.Util

import Data.Foldable
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

-- TODO We'll make this customizable from the command line. Just a
-- default for right now.
defaultRecursionLimit :: Int
defaultRecursionLimit = 1000

valueToTerm :: Data -> Statement
valueToTerm (Symbol s) = case s of
                           (':':s') -> Literal (Symbol s')
                           _ -> Call (splitQualified $ Id s)
valueToTerm v = Literal v

evalMacrosStmt :: (MonadReader ReadOnlyState m, MonadState EvalState m, MonadError FactorError m) =>
                  Statement -> m ()
evalMacrosStmt (Literal (Symbol s)) = pushStack (Stack.singleton $ Symbol (':':s))
evalMacrosStmt (Literal (FunctionValue (Function v ss))) = do
  ss' <- recursivelyAugmentSeq (Just defaultRecursionLimit) ss
  pushStack (Stack.singleton $ FunctionValue (Function v ss'))
evalMacrosStmt (Literal d) = pushStack (Stack.singleton d)
evalMacrosStmt (Call v) = do
  -- We convert lookup errors into Maybe here, since we want macros to
  -- be able to take symbol names that don't exist. If the macro
  -- doesn't replace it with a name that does, then we'll get a
  -- compile error during the type-checking phase.
  value <- errorToMaybe (ask >>= lookupFn v)
  case value of
    Nothing -> defaultBehavior
    Just (UDFunction {}) -> defaultBehavior
    Just (BIFunction {}) -> defaultBehavior
    Just (UDMacro _ (Macro _ ss)) -> evalSeq ss
    Just (ModuleValue {}) -> defaultBehavior
    Just (ModuleSynonym {}) -> defaultBehavior
    Just (TraitValue {}) -> defaultBehavior
   where defaultBehavior = pushStack (Stack.singleton $ Symbol $ qidName v)

evalMacrosSeq :: (MonadReader ReadOnlyState m, MonadState EvalState m, MonadError FactorError m) =>
                 Sequence -> m ()
evalMacrosSeq (Sequence xs) = mapM_ evalMacrosStmt xs

augmentSeqWithMacros :: (MonadReader ReadOnlyState m, MonadError FactorError m) =>
                        Sequence -> m Sequence
augmentSeqWithMacros ss = do
  result <- execStateT (evalMacrosSeq ss) newState
  let newseq = toList $ Stack.FromBottom (stateStack result)
  return $ Sequence (fmap valueToTerm newseq)

recursivelyAugmentSeq :: (MonadReader ReadOnlyState m, MonadError FactorError m) =>
                         Maybe Int -> Sequence -> m Sequence
recursivelyAugmentSeq Nothing ss = augmentSeqWithMacros ss >>=
                                   \ss' -> if ss' == ss then
                                               pure ss'
                                           else
                                               recursivelyAugmentSeq Nothing ss'
recursivelyAugmentSeq (Just n) ss =
    if n < 0 then
        throwError $ MacroRecursionLimit ss
    else
        augmentSeqWithMacros ss >>=
        \ss' -> if ss' == ss then
                    pure ss'
                else
                    recursivelyAugmentSeq (Just $ n - 1) ss'

augmentWithMacros :: (MonadReader ReadOnlyState m, MonadError FactorError m) =>
                     ReaderValue -> m ReaderValue
augmentWithMacros rv =
    case rv of
      UDFunction p (Function v ss) -> (UDFunction p . Function v) <$>
                                      recursivelyAugmentSeq (Just defaultRecursionLimit) ss
      BIFunction {} -> pure rv
      UDMacro {} -> pure rv -- TODO Augment this with macros
      ModuleValue {} -> pure rv
      ModuleSynonym {} -> pure rv
      TraitValue {} -> pure rv
