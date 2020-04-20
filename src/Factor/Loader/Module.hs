{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Factor.Loader.Module where

import Factor.State.Reader
import Factor.State.Resource
import Factor.Id
import Factor.Util
import Factor.Util.Graph(Graph, Cycle(..))
import qualified Factor.Util.Graph as Graph
import Factor.Error
import Factor.Trait.Types
import Factor.Trait.Functor

import Control.Monad.Except
import Control.Monad.State
import Control.Lens
import qualified Data.Map as Map

-- Right now, we need no additional information.
newtype GraphEdge = GraphEdge QId
    deriving (Show, Eq, Ord)

resolveModuleDecl :: (MonadError FactorError m, MonadState ReadOnlyState m) =>
                     QId -> ModuleDecl -> Module -> m Module
resolveModuleDecl mqid decl m =
    case decl of
      ModuleSynonym i (Left dest) -> view' (possibly $ atQIdResource dest) >>= \case
                                     Nothing -> throwError (NoSuchModule dest)
                                     Just r -> forOf (moduleNames.at i) m $ \case
                                        Nothing -> pure (Just r)
                                        Just _ -> throwError (DuplicateDecl i)
      ModuleSynonym i (Right (TraitRef dest args)) -> view' (possibly $ atQId dest) >>= \case
                                        Just (FunctorValue pm) -> do
                                          rid <- bindModule (mqid <> QId [i]) dest pm args
                                          forOf (moduleNames.at i) m $ \case
                                                Nothing -> pure (Just rid)
                                                Just _ -> throwError (DuplicateDecl i)
                                        _ -> throwError (NoSuchModule dest)
      IncludeModule dest -> view' (possibly $ atQId dest) >>= \case
                            Just (ModuleValue m') ->
                                let go m1 (k, v) = forOf (moduleNames.at k) m1 $ \case
                                                   Nothing -> pure (Just v)
                                                   Just _ -> throwError (DuplicateDecl k)
                                in foldM go m (Map.toList $ m'^.moduleNames)
                            _ -> throwError (NoSuchModule dest)
      Alias _ _ -> pure m
      Open _ -> pure m
      AssertTrait _ -> pure m
    where view' x = view x <$> get

loadModule :: (MonadError FactorError m, MonadState ReadOnlyState m) => QId -> ReaderValue -> m ReaderValue
loadModule mqid r =
    case r of
      UDFunction {} -> pure r
      BIFunction {} -> pure r
      UDMacro {} -> pure r
      ModuleValue m -> ModuleValue <$> foldM (flip $ resolveModuleDecl mqid) m (m^.moduleDecls)
      TraitValue {} -> pure r
      FunctorValue {} -> pure r -- TODO Nothing to do here right now, but there will be stuff later.

loadModuleAt :: MonadError FactorError m => QId -> ReadOnlyState -> m ReadOnlyState
loadModuleAt qid r =
  case r^.possibly (atQId qid) of
    Nothing -> throwError (NoSuchModule qid)
    Just rv -> do
      (rv', r') <- runStateT (loadModule qid rv) r
      return $ set (atQId qid) rv' r'

dependenciesFromModuleDecl :: ModuleDecl -> [GraphEdge]
dependenciesFromModuleDecl (ModuleSynonym _ (Left dest)) = [GraphEdge dest' | dest' <- allPrefixes dest]
dependenciesFromModuleDecl (ModuleSynonym _ (Right (TraitRef dest _))) = [GraphEdge dest' | dest' <- allPrefixes dest]
dependenciesFromModuleDecl (IncludeModule dest) = [GraphEdge dest' | dest' <- allPrefixes dest]
dependenciesFromModuleDecl (Alias _ _) = []
dependenciesFromModuleDecl (Open _) = []
dependenciesFromModuleDecl (AssertTrait _) = []

produceModuleDepGraph :: [QId] -> ReadOnlyState -> Graph QId GraphEdge
produceModuleDepGraph qids reader =
    Graph.fromEdges qids (concat . mapWithQId go $ reader^.readerResources) proj
    where proj (GraphEdge q) = q
          go (qid, r) =
              let parents = [(qid, GraphEdge qid') | qid' <- nonemptyPrefixes qid]
                  others = case r of
                             UDFunction {} -> []
                             BIFunction {} -> []
                             UDMacro {} -> []
                             ModuleValue m -> [(qid, e) | decl <- m^.moduleDecls
                                                        , e <- dependenciesFromModuleDecl decl]
                             TraitValue {} -> []
                             FunctorValue {} -> [] -- TODO This
              in parents ++ others

-- As in Factor.Loader.Graph, we reverse the top sort order since we
-- want to load dependencies BEFORE the things that depend upon them.
determineValidLoadOrder :: MonadError FactorError m => Graph QId GraphEdge -> m [QId]
determineValidLoadOrder g
    | (Cycle vs _ : _) <- Graph.findCycles g = throwError (LoadCycle vs)
    | otherwise = case Graph.topSort g of
                    Nothing -> throwError $ InternalError "Error in determineValidLoadOrder"
                    Just order -> pure $ reverse order

loadModules :: MonadError FactorError m => [QId] -> ReadOnlyState -> m ReadOnlyState
loadModules qids r = do
  let graph = produceModuleDepGraph qids r
  loadOrder <- determineValidLoadOrder graph
  foldM (flip loadModuleAt) r loadOrder
