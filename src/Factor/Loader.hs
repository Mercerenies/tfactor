{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Factor.Loader where

import Factor.State.Reader
import Factor.State.Macro
import Factor.Type.Checker
import Factor.Error
import Factor.Id
import Factor.Loader.Graph
import Factor.Loader.Type
import Factor.Trait
import Factor.Trait.Argument

import Control.Monad.Except
import Control.Monad.Reader
import Control.Lens

-- TODO If the canonical name of a thing and the name we're using to
-- access it don't match, then we probably shouldn't load (or resolve
-- aliases, for that matter) as the scoping is going to be all wrong.
-- It may be okay. Think about it, and about the order in which things
-- happen.

validateTraits :: (MonadError FactorError m, MonadReader ReadOnlyState m) => QId -> Module -> m ()
validateTraits qid0 m =
    forM_ (m^.moduleDecls) $ \case
        AssertTrait (TraitRef qid args) ->
            ask >>= lookupFn qid >>= \case
                TraitValue pt -> bindTraitAndNormalize qid pt args >>= \t -> moduleSatisfies' t qid0 m
                _ -> throwError (NoSuchTrait qid)
        Alias _ _ -> pure ()
        Open _ -> pure ()
        IncludeModule _ -> pure ()

loadEntity :: (MonadError FactorError m, MonadReader ReadOnlyState m) => QId -> ReaderValue -> m ReaderValue
loadEntity qid r = do
  checkTypeOf MacroPass r
  r' <- augmentWithMacros r
  checkTypeOf FunctionPass r'
  case r of
    ModuleValue m -> validateTraits qid m
    _ -> pure ()
  pure r'

loadEntityAt :: MonadError FactorError m => QId -> ReadOnlyState -> m ReadOnlyState
loadEntityAt qid r = traverseOf (atQId qid) (\v -> runReaderT (loadEntity qid v) r) r

loadEntities :: MonadError FactorError m => [QId] -> ReadOnlyState -> m ReadOnlyState
loadEntities qids r = do
  loadOrder <- determineLoadOrderFor qids r
  foldM (flip loadEntityAt) r loadOrder
