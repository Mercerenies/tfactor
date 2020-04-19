{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Factor.Loader.Module where

import Factor.State.Reader
import Factor.State.Resource
import Factor.Id
import Factor.Util
import Factor.Util.Graph(Graph, Cycle(..))
import qualified Factor.Util.Graph as Graph
import Factor.Error

import Control.Monad.Except
import Control.Monad.Reader hiding (reader)
import Control.Lens
import qualified Data.Map as Map

-- Right now, we need no additional information.
newtype GraphEdge = GraphEdge QId
    deriving (Show, Eq, Ord)

resolveModuleDecl :: (MonadError FactorError m, MonadReader ReadOnlyState m) =>
                     ModuleDecl -> Module -> m Module
resolveModuleDecl decl m =
    case decl of
      ModuleSynonym i dest -> view (possibly $ atQIdResource dest) >>= \case
                              Nothing -> throwError (NoSuchModule dest)
                              Just r -> forOf (moduleNames.at i) m $ \case
                                        Nothing -> pure (Just r)
                                        Just _ -> throwError (DuplicateDecl i)
      IncludeModule dest -> view (possibly $ atQId dest) >>= \case
                            Just (ModuleValue m') ->
                                let go m1 (k, v) = forOf (moduleNames.at k) m1 $ \case
                                                   Nothing -> pure (Just v)
                                                   Just _ -> throwError (DuplicateDecl k)
                                in foldM go m (Map.toList $ m'^.moduleNames)
                            _ -> throwError (NoSuchModule dest)
      Alias _ _ -> pure m
      Open _ -> pure m
      AssertTrait _ -> pure m

loadModule :: (MonadError FactorError m, MonadReader ReadOnlyState m) => ReaderValue -> m ReaderValue
loadModule r =
    case r of
      UDFunction {} -> pure r
      BIFunction {} -> pure r
      UDMacro {} -> pure r
      ModuleValue m -> ModuleValue <$> foldM (flip resolveModuleDecl) m (m^.moduleDecls)
      TraitValue {} -> pure r

loadModuleAt :: MonadError FactorError m => QId -> ReadOnlyState -> m ReadOnlyState
loadModuleAt qid r = traverseOf (atQId qid) (\v -> runReaderT (loadModule v) r) r

dependenciesFromModuleDecl :: ModuleDecl -> [GraphEdge]
dependenciesFromModuleDecl (ModuleSynonym _ dest) = [GraphEdge dest' | dest' <- allPrefixes dest]
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
