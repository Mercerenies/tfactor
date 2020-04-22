{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Factor.Loader.Type where

import Factor.Type
import Factor.State.Reader
import qualified Factor.Stack as Stack
import Factor.Error
import Factor.Trait
import Factor.Trait.Argument
import Factor.Trait.Types
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

normalizeTypesTraitInfo :: (MonadReader ReadOnlyState m, MonadError FactorError m) =>
                           Map Id Trait -> TraitInfo -> m TraitInfo
normalizeTypesTraitInfo names (TraitFunction p) = TraitFunction <$> normalizePolyFnType names p
normalizeTypesTraitInfo names (TraitMacro p) = TraitMacro <$> normalizePolyFnType names p
normalizeTypesTraitInfo names (TraitModule xs) =
    TraitModule <$> mapM (\(i, t) -> ((,) i) <$> normalizeTypesTraitInfo names t) xs
normalizeTypesTraitInfo names (TraitInclude (TraitRef q args)) = do
  args' <- forM args $ \t -> normalizeType names (NamedType t) >>= \case
              NamedType t' -> pure t'
              _ -> error "Internal error (shape changed) in normalizeTypesTraitInfo"
  return $ TraitInclude (TraitRef q args')
normalizeTypesTraitInfo _ TraitDemandType = pure $ TraitDemandType

normalizeTypesTrait :: (MonadReader ReadOnlyState m, MonadError FactorError m) =>
                       Map Id Trait -> Trait -> m Trait
normalizeTypesTrait names (Trait xs) =
    Trait <$> mapM (\(i, t) -> ((,) i) <$> normalizeTypesTraitInfo names t) xs

normalizeTypesFunctorInfo :: (MonadReader ReadOnlyState m, MonadError FactorError m) =>
                             Map Id Trait -> FunctorInfo -> m FunctorInfo
normalizeTypesFunctorInfo names (FunctorUDFunction t f) =
    FunctorUDFunction <$> normalizePolyFnType names t <*> pure f
normalizeTypesFunctorInfo names (FunctorUDMacro t f) =
    FunctorUDMacro <$> normalizePolyFnType names t <*> pure f
normalizeTypesFunctorInfo names (FunctorModule inner) = FunctorModule <$> normalizeTypesFunctor names inner
normalizeTypesFunctorInfo names (FunctorTrait (ParameterizedTrait args t)) = do
  reader <- ask
  names' <- forM args $ \(ModuleArg i (TraitRef q innerargs)) -> lookupFn q reader >>= \case
           TraitValue pt -> fmap ((,) i) $ bindTraitAndNormalize q pt innerargs
           _ -> throwError (NoSuchTrait q)
  let names'' = Map.fromList names' <> names
  t' <- normalizeTypesTrait names'' t
  return (FunctorTrait (ParameterizedTrait args t'))
normalizeTypesFunctorInfo _ FunctorDemandType = pure $ FunctorDemandType

normalizeTypesFunctor :: (MonadReader ReadOnlyState m, MonadError FactorError m) =>
                         Map Id Trait -> Map Id FunctorInfo -> m (Map Id FunctorInfo)
normalizeTypesFunctor names = mapM (normalizeTypesFunctorInfo names)

normalizeTypesRes :: (MonadReader ReadOnlyState m, MonadError FactorError m) => ReaderValue -> m ReaderValue
normalizeTypesRes (UDFunction t f) = UDFunction <$> normalizePolyFnType Map.empty t <*> pure f
normalizeTypesRes (BIFunction t f) = BIFunction <$> normalizePolyFnType Map.empty t <*> pure f
normalizeTypesRes (UDMacro t f) = UDMacro <$> normalizePolyFnType Map.empty t <*> pure f
normalizeTypesRes (ModuleValue v) = pure $ ModuleValue v
normalizeTypesRes (TraitValue (ParameterizedTrait args t)) = do
  reader <- ask
  names <- forM args $ \(ModuleArg i (TraitRef q innerargs)) -> lookupFn q reader >>= \case
           TraitValue pt -> fmap ((,) i) $ bindTraitAndNormalize q pt innerargs
           _ -> throwError (NoSuchTrait q)
  let names' = Map.fromList names
  t' <- normalizeTypesTrait names' t
  return $ TraitValue (ParameterizedTrait args t')
normalizeTypesRes (FunctorValue (ParameterizedModule args t)) = do
  reader <- ask
  names <- forM args $ \(ModuleArg i (TraitRef q innerargs)) -> lookupFn q reader >>= \case
           TraitValue pt -> fmap ((,) i) $ bindTraitAndNormalize q pt innerargs
           _ -> throwError (NoSuchTrait q)
  -- TODO QId [].... yeah
  let selfptrait = functorToTrait (QId []) (ParameterizedModule args t)
      selftrait = bindTraitUnchecked (QId []) selfptrait (fmap (\(ModuleArg i _) -> QId [i]) args)
      names' = Map.singleton (Id "Self") selftrait <> Map.fromList names
  t' <- normalizeTypesFunctor names' t
  return $ FunctorValue (ParameterizedModule args t')

normalizeTypesAt :: MonadError FactorError m => QId -> ReadOnlyState -> m ReadOnlyState
normalizeTypesAt qid r = traverseOf (atQId qid) (\v -> runReaderT (normalizeTypesRes v) r) r

normalizeAllTypes :: MonadError FactorError m => [QId] -> ReadOnlyState -> m ReadOnlyState
normalizeAllTypes qids r = foldM (flip normalizeTypesAt) r qids

bindTraitAndNormalize :: (MonadReader ReadOnlyState m, MonadError FactorError m) =>
                         QId -> ParameterizedTrait -> [QId] -> m Trait
bindTraitAndNormalize qid pt args = bindTrait qid pt args >>= normalizeTypesTrait Map.empty

functorToTrait :: QId -> ParameterizedModule -> ParameterizedTrait
functorToTrait qid0 (ParameterizedModule params info0) =
    ParameterizedTrait params (toTrait qid0 info0)
        where toTrait qid info =
                  let go _ (FunctorUDFunction p _) = [TraitFunction p]
                      go _ (FunctorUDMacro p _) = [TraitMacro p]
                      go i (FunctorModule info') =
                          let qid' = qid <> QId [i]
                              Trait inner = toTrait qid' info'
                          in [TraitModule inner]
                      go _ (FunctorTrait {}) = [] -- TODO These can't appear in traits right
                                                  -- now. Will this be supported later?
                      go _ FunctorDemandType = [TraitDemandType]
                  in Trait $ concatMap (\(i, v) -> map ((,) i) $ go i v) (Map.toList info)
