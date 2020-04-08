{-# LANGUAGE FlexibleContexts, ViewPatterns, RankNTypes,
  ConstraintKinds, KindSignatures, TemplateHaskell #-}

module Factor.State(EvalState(..), ReadOnlyState(ReadOnlyState), ReaderValue(..),
                    BuiltIn(..), BuiltInConstraints,
                    readerNames,
                    newState, newReader,
                    pushStack, peekStackMaybe, peekStack, popStackMaybe, popStack,
                    declsToReadOnly, lookupFn,
                    readerFunctionType, readerMacroType) where

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
                 | UDMacro PolyFunctionType  Macro       -- User-defined macro
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

declsToReadOnly :: MonadError FactorError m =>
                   [Declaration] -> Map Id ReaderValue -> m (Map Id ReaderValue)
declsToReadOnly ds r = foldM go r ds
    where go reader decl =
              case decl of
                FunctionDecl _ (Function Nothing _) ->
                    throwError (InternalError "Unnamed top-level function")
                FunctionDecl t (Function (Just v) def)
                 | Map.member v reader -> throwError (DuplicateDecl v)
                 | otherwise -> pure $ defineFunction v t def reader
                MacroDecl t (Macro v def)
                 | Map.member v reader -> throwError (DuplicateDecl v)
                 | otherwise -> pure $ defineMacro v t def reader
                ModuleDecl v def
                 | Map.member v reader -> throwError (DuplicateDecl v)
                 | otherwise -> do
                          inner <- foldM go Map.empty def
                          pure $ defineModule v inner reader

defineFunction :: Id -> PolyFunctionType -> Sequence -> Map Id ReaderValue -> Map Id ReaderValue
defineFunction v t def = Map.insert v (UDFunction t $ Function (Just v) def)

defineMacro :: Id -> PolyFunctionType -> Sequence -> Map Id ReaderValue -> Map Id ReaderValue
defineMacro v t def = Map.insert v (UDMacro t $ Macro v def)

defineModule :: Id -> Map Id ReaderValue -> Map Id ReaderValue -> Map Id ReaderValue
defineModule v def = Map.insert v (Module def)

lookupFn :: MonadError FactorError m => QId -> ReadOnlyState -> m ReaderValue
lookupFn (QId ids) reader =
  -- Lookup the top-level name in the alias table first.
  let go (Module names) i = maybe (throwError $ NoSuchFunction (QId ids)) pure $ Map.lookup i names
      go _ _ = throwError $ NoSuchModule (QId ids)
  in foldM go (Module $ view readerNames reader) ids

readerFunctionType :: ReaderValue -> Maybe PolyFunctionType
readerFunctionType (UDFunction t _) = Just t
readerFunctionType (BIFunction t _) = Just t
readerFunctionType (UDMacro _ _) = Nothing
readerFunctionType (Module _) = Nothing

readerMacroType :: ReaderValue -> Maybe PolyFunctionType
readerMacroType (UDFunction _ _) = Nothing
readerMacroType (BIFunction _ _) = Nothing
readerMacroType (UDMacro t _) = Just t
readerMacroType (Module _) = Nothing
