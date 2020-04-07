{-# LANGUAGE FlexibleContexts, ViewPatterns, RankNTypes, ConstraintKinds, KindSignatures #-}

module Factor.State(EvalState(..), ReadOnlyState(..), ReaderValue(..),
                    BuiltIn(..), BuiltInConstraints,
                    newState, newReader,
                    pushStack, peekStackMaybe, peekStack, popStackMaybe, popStack,
                    declsToReadOnly, lookupFn, lookupFn',
                    readerFunctionType) where

import Factor.Error
import Factor.Code
import Factor.Id
import Factor.Type
import Factor.Stack(Stack)
import qualified Factor.Stack as Stack

import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad.Reader hiding (reader)
import Control.Monad.State
import Control.Monad.Except

data EvalState = EvalState {
      stateStack :: Stack Data
    } deriving (Show)

data ReadOnlyState = ReadOnlyState {
      readerNames :: Map Id ReaderValue
    }

data ReaderValue = UDFunction PolyFunctionType  Function    -- User-defined function
                 | BIFunction PolyFunctionType (BuiltIn ()) -- Built-in function

type BuiltInConstraints m = (MonadReader ReadOnlyState m, MonadState EvalState m, MonadError FactorError m)

newtype BuiltIn a = BuiltIn { unBuiltIn :: forall m. BuiltInConstraints m => m a }

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
                    Just (a, b) -> Just a <$ modify (\s -> s { stateStack = b })

popStack :: (MonadState EvalState m, MonadError FactorError m) => Int -> m (Stack Data)
popStack n = popStackMaybe n >>= maybe (throwError StackUnderflow) return

declsToReadOnly :: MonadError FactorError m => [Declaration] -> ReadOnlyState -> m ReadOnlyState
declsToReadOnly ds r = foldM go r ds
    where go reader decl =
              case decl of
                FunctionDecl _ (Function Nothing _) ->
                    throwError (InternalError "Unnamed top-level function")
                FunctionDecl t (Function (Just v) def)
                 | Map.member v (readerNames reader) -> throwError (DuplicateDecl v)
                 | otherwise -> pure $ defineFunction v t def reader
          defineFunction v t def reader =
              reader { readerNames =
                           Map.insert v (UDFunction t $ Function (Just v) def)
                                  (readerNames reader) }

-- //// Rewrite these correctly once we have modules

lookupFn :: QId -> ReadOnlyState -> Maybe ReaderValue
lookupFn (QId (last -> v)) (readerNames -> fns) = Map.lookup v fns

lookupFn' :: MonadError FactorError m => QId -> ReadOnlyState -> m ReaderValue
lookupFn' v r = maybe (throwError $ NoSuchFunction v) pure $ lookupFn v r

readerFunctionType :: ReaderValue -> PolyFunctionType
readerFunctionType (UDFunction t _) = t
readerFunctionType (BIFunction t _) = t
