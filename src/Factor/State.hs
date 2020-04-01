{-# LANGUAGE FlexibleContexts, ViewPatterns #-}

module Factor.State(EvalState(..), ReadOnlyState(..), newState, newReader,
                    pushStack, peekStackMaybe, peekStack, popStackMaybe, popStack,
                    declsToReadOnly, lookupFn) where

import Factor.Error
import Factor.Code
import Factor.Id

import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except

data EvalState = EvalState {
      stateStack :: [Data]
    } deriving (Show)

data ReadOnlyState = ReadOnlyState {
      readerFunctions :: Map Id Function
    } deriving (Show)

newState :: EvalState
newState = EvalState []

newReader :: ReadOnlyState
newReader = ReadOnlyState Map.empty

pushStack :: MonadState EvalState m => [Data] -> m ()
pushStack xs = modify $ \s -> s { stateStack = xs ++ stateStack s }

peekStackMaybe :: MonadState EvalState m => Int -> m (Maybe [Data])
peekStackMaybe n = extractFirstN <$> gets stateStack
    where extractFirstN stack = take n stack <$ guard (length stack >= n)

peekStack :: (MonadState EvalState m, MonadError FactorError m) => Int -> m [Data]
peekStack n = peekStackMaybe n >>= maybe (throwError StackUnderflow) return

popStackMaybe :: MonadState EvalState m => Int -> m (Maybe [Data])
popStackMaybe n = gets stateStack >>= extractFirstN
    where extractFirstN stack = if length stack < n then
                                    return Nothing
                                else
                                    let (h, t) = splitAt n stack
                                    in Just h <$ modify (\s -> s { stateStack = t })

popStack :: (MonadState EvalState m, MonadError FactorError m) => Int -> m [Data]
popStack n = popStackMaybe n >>= maybe (throwError StackUnderflow) return

declsToReadOnly :: MonadError FactorError m => [Declaration] -> m ReadOnlyState
declsToReadOnly = foldM go newReader
    where go reader decl =
              case decl of
                FunctionDecl (Function Nothing _) ->
                    throwError (InternalError "Unnamed top-level function")
                FunctionDecl (Function (Just v) def)
                 | Map.member v (readerFunctions reader) -> throwError (DuplicateFunctionDecl v)
                 | otherwise -> pure $ defineFunction v def reader
          defineFunction v def reader =
              reader { readerFunctions =
                           Map.insert v (Function (Just v) def) (readerFunctions reader) }

lookupFn :: Id -> ReadOnlyState -> Maybe Function
lookupFn v (readerFunctions -> fns) = Map.lookup v fns
