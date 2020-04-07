{-# LANGUAGE FlexibleContexts, ViewPatterns, RankNTypes,
  ConstraintKinds, KindSignatures, TemplateHaskell #-}

module Factor.State(EvalState(..), ReadOnlyState(..), ReaderValue(..),
                    BuiltIn(..), BuiltInConstraints,
                    readerNames,
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
import Control.Lens

data EvalState = EvalState {
      stateStack :: Stack Data
    } deriving (Show)

data ReadOnlyState = ReadOnlyState {
      _readerNames :: Map Id ReaderValue
    }

data ReaderValue = UDFunction PolyFunctionType  Function    -- User-defined function
                 | BIFunction PolyFunctionType (BuiltIn ()) -- Built-in function
                 | Module (Map Id ReaderValue)

type BuiltInConstraints m = (MonadReader ReadOnlyState m, MonadState EvalState m, MonadError FactorError m)

newtype BuiltIn a = BuiltIn { unBuiltIn :: forall m. BuiltInConstraints m => m a }

makeLenses ''ReadOnlyState

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
                 | Map.member v (view readerNames reader) -> throwError (DuplicateDecl v)
                 | otherwise -> pure $ defineFunction v t def reader
                ModuleDecl v def
                 | Map.member v (view readerNames reader) -> throwError (DuplicateDecl v)
                 | otherwise -> do
                          ReadOnlyState inner <- foldM go newReader def
                          pure $ defineModule v inner reader

defineFunction :: Id -> PolyFunctionType -> Sequence -> ReadOnlyState -> ReadOnlyState
defineFunction v t def = over readerNames $ Map.insert v (UDFunction t $ Function (Just v) def)

defineModule :: Id -> Map Id ReaderValue -> ReadOnlyState -> ReadOnlyState
defineModule v def = over readerNames $ Map.insert v (Module def)

lookupFn :: QId -> ReadOnlyState -> Maybe ReaderValue
lookupFn (QId ids) (view readerNames -> names0) = foldM go (Module names0) ids
    where go (Module names) i = Map.lookup i names
          go _ _ = Nothing

lookupFn' :: MonadError FactorError m => QId -> ReadOnlyState -> m ReaderValue
lookupFn' v r = maybe (throwError $ NoSuchFunction v) pure $ lookupFn v r

readerFunctionType :: ReaderValue -> Maybe PolyFunctionType
readerFunctionType (UDFunction t _) = Just t
readerFunctionType (BIFunction t _) = Just t
readerFunctionType (Module _) = Nothing
