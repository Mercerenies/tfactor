{-# LANGUAGE FlexibleContexts #-}

module Factor.Loader.Type where

import Factor.Type
import Factor.State
import qualified Factor.Stack as Stack
import Factor.Error
import Factor.Trait.Types
import Factor.Id

import Control.Monad.Reader
import Control.Monad.Except
import Control.Lens

normalizeType :: (MonadReader ReadOnlyState m, MonadError FactorError m) => Type -> m Type
normalizeType (FunType (FunctionType (StackDesc args a) (StackDesc rets r))) = do
  Stack.FromTop args' <- traverse normalizeType (Stack.FromTop args)
  Stack.FromTop rets' <- traverse normalizeType (Stack.FromTop rets)
  return $ FunType (FunctionType (StackDesc args' a) (StackDesc rets' r))
normalizeType (NamedType qid) = ask >>= \r -> NamedType <$> lookupFnName qid r
normalizeType (GroundVar v) = pure $ GroundVar v
normalizeType (QuantVar v) = pure $ QuantVar v

normalizePolyFnType :: (MonadReader ReadOnlyState m, MonadError FactorError m) =>
                       PolyFunctionType -> m PolyFunctionType
normalizePolyFnType (PolyFunctionType names (FunctionType (StackDesc args a) (StackDesc rets r))) = do
  Stack.FromTop args' <- traverse normalizeType (Stack.FromTop args)
  Stack.FromTop rets' <- traverse normalizeType (Stack.FromTop rets)
  return $ PolyFunctionType names (FunctionType (StackDesc args' a) (StackDesc rets' r))

normalizeTypesTrait :: (MonadReader ReadOnlyState m, MonadError FactorError m) =>
                       TraitInfo -> m TraitInfo
normalizeTypesTrait (TraitFunction p) = TraitFunction <$> normalizePolyFnType p
normalizeTypesTrait (TraitMacro p) = TraitMacro <$> normalizePolyFnType p
normalizeTypesTrait (TraitModule xs) =
    TraitModule <$> mapM (\(i, t) -> ((,) i) <$> normalizeTypesTrait t) xs
normalizeTypesTrait (TraitInclude q) = pure $ TraitInclude q

normalizeTypesRes :: (MonadReader ReadOnlyState m, MonadError FactorError m) => ReaderValue -> m ReaderValue
normalizeTypesRes (UDFunction t f) = UDFunction <$> normalizePolyFnType t <*> pure f
normalizeTypesRes (BIFunction t f) = BIFunction <$> normalizePolyFnType t <*> pure f
normalizeTypesRes (UDMacro t f) = UDMacro <$> normalizePolyFnType t <*> pure f
normalizeTypesRes (ModuleValue v) = pure $ ModuleValue v
normalizeTypesRes (TraitValue (Trait xs)) =
    TraitValue . Trait <$> mapM (\(i, t) -> ((,) i) <$> normalizeTypesTrait t) xs

normalizeTypesAt :: MonadError FactorError m => QId -> ReadOnlyState -> m ReadOnlyState
normalizeTypesAt qid r = traverseOf (atQId qid) (\v -> runReaderT (normalizeTypesRes v) r) r

normalizeAllTypes :: MonadError FactorError m => [QId] -> ReadOnlyState -> m ReadOnlyState
normalizeAllTypes qids r = foldM (flip normalizeTypesAt) r qids
