{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Factor.Loader.Type where

import Factor.Type
import Factor.State
import qualified Factor.Stack as Stack
import Factor.Error
import Factor.Trait
import Factor.Trait.Argument
import Factor.Id

import Control.Monad.Reader hiding (reader)
import Control.Monad.Except
import Control.Lens
import Data.Map(Map)
import qualified Data.Map as Map

normalizeType :: (MonadReader ReadOnlyState m, MonadError FactorError m) =>
                 Map Id Trait -> Type -> m Type
normalizeType names (FunType (FunctionType (StackDesc args a) (StackDesc rets r))) = do
  Stack.FromTop args' <- traverse (normalizeType names) (Stack.FromTop args)
  Stack.FromTop rets' <- traverse (normalizeType names) (Stack.FromTop rets)
  return $ FunType (FunctionType (StackDesc args' a) (StackDesc rets' r))
normalizeType names (NamedType qid)
    | QId (first:rest) <- qid
    , Just t <- Map.lookup first names = ask >>=
                                         \r -> nestedTraitDeep r t (QId rest) >>=
                                         \t' -> traitDemandsType r t' >>=
                                         \b -> if b then pure (NamedType qid) else throwError (TraitError $ MissingFromTrait qid TraitDemandType)
    | otherwise = ask >>= \r -> NamedType <$> lookupFnName qid r
normalizeType _ (GroundVar v) = pure $ GroundVar v
normalizeType _ (QuantVar v) = pure $ QuantVar v

normalizePolyFnType :: (MonadReader ReadOnlyState m, MonadError FactorError m) =>
                       Map Id Trait -> PolyFunctionType -> m PolyFunctionType
normalizePolyFnType names (PolyFunctionType quants (FunctionType (StackDesc args a) (StackDesc rets r))) = do
  Stack.FromTop args' <- traverse (normalizeType names) (Stack.FromTop args)
  Stack.FromTop rets' <- traverse (normalizeType names) (Stack.FromTop rets)
  return $ PolyFunctionType quants (FunctionType (StackDesc args' a) (StackDesc rets' r))

normalizeTypesTrait :: (MonadReader ReadOnlyState m, MonadError FactorError m) =>
                       Map Id Trait -> TraitInfo -> m TraitInfo
normalizeTypesTrait names  (TraitFunction p) = TraitFunction <$> normalizePolyFnType names p
normalizeTypesTrait names (TraitMacro p) = TraitMacro <$> normalizePolyFnType names p
normalizeTypesTrait names (TraitModule xs) =
    TraitModule <$> mapM (\(i, t) -> ((,) i) <$> normalizeTypesTrait names t) xs
normalizeTypesTrait _ (TraitInclude q) = pure $ TraitInclude q -- TODO Normalize traits in arguments
normalizeTypesTrait _ TraitDemandType = pure $ TraitDemandType

normalizeTypesRes :: (MonadReader ReadOnlyState m, MonadError FactorError m) => ReaderValue -> m ReaderValue
normalizeTypesRes (UDFunction t f) = UDFunction <$> normalizePolyFnType Map.empty t <*> pure f
normalizeTypesRes (BIFunction t f) = BIFunction <$> normalizePolyFnType Map.empty t <*> pure f
normalizeTypesRes (UDMacro t f) = UDMacro <$> normalizePolyFnType Map.empty t <*> pure f
normalizeTypesRes (ModuleValue v) = pure $ ModuleValue v
normalizeTypesRes (TraitValue (ParameterizedTrait args (Trait xs))) = do
  reader <- ask
  names <- forM args $ \(ModuleArg i q) -> lookupFn q reader >>= \case
           TraitValue (ParameterizedTrait [] t) -> pure (i, t)
           TraitValue (ParameterizedTrait ys _) -> throwError (TraitArgError q (length ys) 0) -- TODO This
           _ -> throwError (NoSuchTrait q)
  let names' = Map.fromList names
  xs' <- mapM (\(i, t) -> ((,) i) <$> normalizeTypesTrait names' t) xs
  return $ TraitValue (ParameterizedTrait args (Trait xs'))

normalizeTypesAt :: MonadError FactorError m => QId -> ReadOnlyState -> m ReadOnlyState
normalizeTypesAt qid r = traverseOf (atQId qid) (\v -> runReaderT (normalizeTypesRes v) r) r

normalizeAllTypes :: MonadError FactorError m => [QId] -> ReadOnlyState -> m ReadOnlyState
normalizeAllTypes qids r = foldM (flip normalizeTypesAt) r qids
