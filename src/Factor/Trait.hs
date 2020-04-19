{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, LambdaCase #-}

module Factor.Trait(Trait(..), ParameterizedTrait(..), TraitInfo(..), UnsatisfiedTrait(..),
                    FromUnsatisfiedTrait(..),
                    traitDemandsType, mergeTraits, nestedTrait, nestedTraitDeep,
                    moduleSatisfies, moduleSatisfies') where

import Factor.Trait.Types
import Factor.State
import Factor.Error
import Factor.Id
import Factor.Util
import Factor.Type
import Factor.Type.Error
import Factor.Type.Unify

import Data.Monoid
import Control.Monad.Reader hiding (reader)
import Control.Monad.Except
import Control.Monad.Writer
import Control.Lens

requireSubtype :: (FromUnsatisfiedTrait e, MonadError e m) => QId -> TraitInfo -> Type -> Type -> m ()
requireSubtype q t a b =
    case runWriterT (a `isSubtypeOf` b) of
      Left (_ :: TypeError) -> throwError (fromUnsatisfiedTrait $ IncompatibleWithTrait q t)
      Right ((), _) -> pure ()

requireExists :: (FromUnsatisfiedTrait e, MonadError e m) => QId -> TraitInfo -> Maybe ReaderValue -> m ReaderValue
requireExists q t Nothing = throwError (fromUnsatisfiedTrait $ MissingFromTrait q t)
requireExists _ _ (Just x) = pure x

traitDemandsType :: MonadError FactorError m => ReadOnlyState -> Trait -> m Bool
traitDemandsType r (Trait xs) = getAny <$> foldMapM go xs
    where go (_, info) =
              case info of
                TraitFunction {} -> pure $ Any False
                TraitMacro {} -> pure $ Any False
                TraitModule {} -> pure $ Any False
                TraitInclude q -> lookupFn q r >>= \case
                                  TraitValue (ParameterizedTrait [] t) -> Any <$> traitDemandsType r t
                                  TraitValue (ParameterizedTrait ys _) ->
                                      -- TODO This
                                      throwError (TraitArgError q (length ys) 0)
                                  _ -> throwError (NoSuchTrait q)
                TraitDemandType -> pure $ Any True

mergeTraits :: Trait -> Trait -> Trait
mergeTraits (Trait xs) (Trait ys) = Trait $ xs ++ ys

nestedTrait :: MonadError FactorError m => ReadOnlyState -> Trait -> Id -> m Trait
nestedTrait r (Trait xs) y = foldMapM check xs >>= \case
                             [] -> throwError (NoSuchTrait (QId [y]))
                             ys -> pure $ foldl mergeTraits (Trait []) ys
    where check (v, info) =
              case info of
                TraitFunction {} -> pure []
                TraitMacro {} -> pure []
                TraitModule m | v == y -> pure [Trait m]
                              | otherwise -> pure []
                TraitInclude q -> lookupFn q r >>= \case
                                  TraitValue (ParameterizedTrait [] t) -> (\x -> [x]) <$> nestedTrait r t y
                                  TraitValue (ParameterizedTrait ys _) ->
                                      -- TODO This
                                      throwError (TraitArgError q (length ys) 0)
                                  _ -> throwError (NoSuchTrait q)
                TraitDemandType -> pure []

nestedTraitDeep :: MonadError FactorError m => ReadOnlyState -> Trait -> QId -> m Trait
nestedTraitDeep r t (QId xs) = foldM (nestedTrait r) t xs

-- TODO We want to verify properties like "does this included trait
-- exist" or "did we pass the right number of arguments to the
-- included trait" before requiring the trait in a module.
moduleSatisfies :: MonadError FactorError m => ReadOnlyState -> Trait -> Module -> m ()
moduleSatisfies reader (Trait reqs0) m0 = mapM_ (go (QId []) m0) reqs0
    where go qid m (v, info) =
              let qid' = qid <> QId [v]
                  value = (m^.moduleNames.possibly (ix v)) >>= (\rid -> reader^.readerResources.possibly (ix rid))
              in case info of
                   TraitFunction (PolyFunctionType r reqtype) -> do
                             value' <- requireExists qid' info value
                             case value' of
                               UDFunction (PolyFunctionType _ decltype) _ ->
                                   requireSubtype qid' info (FunType decltype) (toGround r $ FunType reqtype)
                               BIFunction (PolyFunctionType _ decltype) _ ->
                                   requireSubtype qid' info (FunType decltype) (toGround r $ FunType reqtype)
                               _ -> throwError (TraitError $ MissingFromTrait qid' info)
                   TraitMacro (PolyFunctionType r reqtype) -> do
                             value' <- requireExists qid' info value
                             case value' of
                               UDMacro (PolyFunctionType _ decltype) _ -> -- TODO Support BIMacro here,
                                                                          -- once we write that
                                   requireSubtype qid' info (FunType decltype) (toGround r $ FunType reqtype)
                               _ -> throwError (TraitError $ MissingFromTrait qid' info)
                   TraitModule reqs -> do
                             value' <- requireExists qid' info value
                             case value' of
                               ModuleValue m' -> mapM_ (go qid' m') reqs
                               _ -> throwError (TraitError $ MissingFromTrait qid' info)
                   TraitInclude q -> lookupFn q reader >>= \case
                                     TraitValue (ParameterizedTrait [] t) -> moduleSatisfies reader t m
                                     TraitValue (ParameterizedTrait xs _) ->
                                         -- TODO This
                                         throwError (TraitArgError qid' (length xs) 0)
                                     _ -> throwError (NoSuchTrait q)
                   TraitDemandType -> if m0^.moduleIsType then
                                          pure ()
                                      else
                                          throwError (TraitError $ MissingFromTrait qid info)

moduleSatisfies' :: (MonadReader ReadOnlyState m, MonadError FactorError m) =>
                    Trait -> Module -> m ()
moduleSatisfies' t m = ask >>= \r -> moduleSatisfies r t m
