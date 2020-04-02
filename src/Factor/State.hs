{-# LANGUAGE FlexibleContexts, ViewPatterns #-}

module Factor.State(EvalState(..), ReadOnlyState(..), newState, newReader,
                    pushStack, peekStackMaybe, peekStack, popStackMaybe, popStack,
                    declsToReadOnly, lookupFn, lookupFn') where

import Factor.Error
import Factor.Code
import Factor.Id
import Factor.Type
import Factor.Stack(Stack)
import qualified Factor.Stack as Stack

import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except

data EvalState = EvalState {
      stateStack :: Stack Data
    } deriving (Show)

data ReadOnlyState = ReadOnlyState {
      readerFunctions :: Map Id (FunctionType, Function)
    } deriving (Show)

newState :: EvalState
newState = EvalState Stack.empty

newReader :: ReadOnlyState
newReader = ReadOnlyState Map.empty

pushStack :: MonadState EvalState m => Stack Data -> m ()
pushStack xs = modify $ \s -> s { stateStack = Stack.appendStack xs (stateStack s) }

peekStackMaybe :: MonadState EvalState m => Int -> m (Maybe (Stack Data))
peekStackMaybe n = go <$> gets stateStack
    where go = fmap fst . Stack.splitStack n

peekStack :: (MonadState EvalState m, MonadError FactorError m) => Int -> m (Stack Data)
peekStack n = peekStackMaybe n >>= maybe (throwError StackUnderflow) return

popStackMaybe :: MonadState EvalState m => Int -> m (Maybe (Stack Data))
popStackMaybe n = gets stateStack >>= go
    where go ss = case Stack.splitStack n ss of
                    Nothing -> pure Nothing
                    Just (a, b) -> Just a <$ modify (\s -> s { stateStack = b})

popStack :: (MonadState EvalState m, MonadError FactorError m) => Int -> m (Stack Data)
popStack n = popStackMaybe n >>= maybe (throwError StackUnderflow) return

declsToReadOnly :: MonadError FactorError m => [Declaration] -> m ReadOnlyState
declsToReadOnly = foldM go newReader
    where go reader decl =
              case decl of
                FunctionDecl _ (Function Nothing _) ->
                    throwError (InternalError "Unnamed top-level function")
                FunctionDecl t (Function (Just v) def)
                 | Map.member v (readerFunctions reader) -> throwError (DuplicateFunctionDecl v)
                 | otherwise -> pure $ defineFunction v t def reader
          defineFunction v t def reader =
              reader { readerFunctions =
                           Map.insert v (t, Function (Just v) def) (readerFunctions reader) }

lookupFn :: Id -> ReadOnlyState -> Maybe (FunctionType, Function)
lookupFn v (readerFunctions -> fns) = Map.lookup v fns

lookupFn' :: MonadError FactorError m => Id -> ReadOnlyState -> m (FunctionType, Function)
lookupFn' v r = maybe (throwError $ NoSuchFunction v) pure $ lookupFn v r
