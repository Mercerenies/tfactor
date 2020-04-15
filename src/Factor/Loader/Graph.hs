{-# LANGUAGE FlexibleContexts #-}

module Factor.Loader.Graph where

import Factor.State
import Factor.Code
import Factor.Id
import Factor.Util
import Factor.Util.Graph(Graph, Cycle(..))
import qualified Factor.Util.Graph as Graph
import Factor.Error

import Data.Set(Set)
import qualified Data.Set as Set
--import Data.Map(Map)
import qualified Data.Map as Map
import Data.Foldable
import Control.Monad.Except
import Control.Lens

data DependencyStrength = WeakDependency | StrongDependency
                          deriving (Show, Read, Eq, Ord, Enum)

data GraphEdge = GraphEdge QId DependencyStrength
                 deriving (Show, Eq, Ord)

findDependenciesStmt :: Statement -> Set QId
findDependenciesStmt (Literal (FunctionValue (Function _ ss))) = findDependenciesSeq ss
findDependenciesStmt (Literal {}) = Set.empty
findDependenciesStmt (Call qid) = Set.singleton qid

findDependenciesSeq :: Sequence -> Set QId
findDependenciesSeq (Sequence ss) = foldMap findDependenciesStmt ss

filterAndClassify :: [QId] -> ReadOnlyState -> Set QId -> Set GraphEdge
filterAndClassify qids r = setFilterMap go
    where go qid = case lookupFn qid r of
                     _ | qid `notElem` qids -> Nothing -- Ignore elements we don't care about.
                     Left _ -> Nothing
                     Right (UDFunction {}) -> Just $ GraphEdge qid WeakDependency
                     Right (BIFunction {}) -> Just $ GraphEdge qid WeakDependency
                     Right (UDMacro {}) -> Just $ GraphEdge qid StrongDependency
                     Right (ModuleValue {}) -> Just $ GraphEdge qid WeakDependency -- TODO Not sure about this one?

produceDependencyGraph :: [QId] -> ReadOnlyState -> Graph QId GraphEdge
produceDependencyGraph qids reader =
    Graph.fromEdges qids (fold $ Map.mapWithKey (go' (QId [])) $ reader^.readerNames) proj1
    where proj1 (GraphEdge qid _) = qid
          namesToEdges = toList . filterAndClassify qids reader
          go k0 k1 v = let k = k0 <> QId [k1]
                           edges = case v of
                                     UDFunction _ (Function _ ss) -> namesToEdges $ findDependenciesSeq ss
                                     BIFunction {} -> []
                                     UDMacro _ (Macro _ ss) -> namesToEdges $ findDependenciesSeq ss
                                     ModuleValue {} -> [] -- TODO Right now, modules have no dependencies because they have no load phase
                           inner = case v of
                                     ModuleValue m -> fold (Map.mapWithKey (go' k) (m^.moduleNames))
                                     _ -> []
                       in map ((,) k) edges ++ inner
          go' k0 k1 v = case reader^.readerResources.pre (ix v) of
                          Nothing -> error "Internal error in produceDependencyGraph"
                          Just v' -> go k0 k1 v'

collapseCycles :: MonadError FactorError m =>
                  Graph QId GraphEdge -> m (Graph (Set QId) GraphEdge)
collapseCycles g0 = Graph.removeLoops <$> foldM handleCycle (Graph.liftToSet g0) (Graph.findCycles g0)
    where handleCycle g (Cycle vs es)
              | any (\(GraphEdge _ dep) -> dep == StrongDependency) es = throwError (LoadCycle vs)
              | otherwise = pure (Graph.collapseVertices (Set.fromList vs) g)

-- We reverse the topological sort since the top sort is based on
-- dependencies, and we want to load the dependencies BEFORE the
-- things that depend on them, not after.
determineValidLoadOrder :: Graph (Set QId) GraphEdge -> Maybe [QId]
determineValidLoadOrder = fmap (reverse . flattenCycles) . Graph.topSort
    where flattenCycles = concatMap Set.toList

determineLoadOrderFor :: MonadError FactorError m => [QId] -> ReadOnlyState -> m [QId]
determineLoadOrderFor qids reader = do
  let graph = produceDependencyGraph qids reader
  graph' <- collapseCycles graph
  maybe (throwError $ InternalError "Error in determineLoadOrderFor") pure $
        determineValidLoadOrder graph'
