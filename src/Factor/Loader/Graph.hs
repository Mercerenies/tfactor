{-# LANGUAGE FlexibleContexts #-}

module Factor.Loader.Graph where

import Factor.State
import Factor.Code
import Factor.Id
import Factor.Util
import Factor.Util.Graph(Graph)
import qualified Factor.Util.Graph as Graph

import Data.Set(Set)
import qualified Data.Set as Set
--import Data.Map(Map)
import qualified Data.Map as Map
import Data.Foldable

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

filterAndClassify :: ReadOnlyState -> Set QId -> Set GraphEdge
filterAndClassify r = setFilterMap go
    where go qid = case lookupFn qid r of
                     Left _ -> Nothing
                     Right (UDFunction {}) -> Just $ GraphEdge qid WeakDependency
                     Right (BIFunction {}) -> Just $ GraphEdge qid WeakDependency
                     Right (UDMacro {}) -> Just $ GraphEdge qid StrongDependency
                     Right (Module {}) -> Just $ GraphEdge qid WeakDependency -- TODO Not sure about this one?

produceDependencyGraph :: ReadOnlyState -> Graph QId GraphEdge
produceDependencyGraph (reader @ (ReadOnlyState modl)) =
    Graph.fromEdges (allNames reader) (fold $ Map.mapWithKey (go (QId [])) modl) proj1
    where proj1 (GraphEdge qid _) = qid
          namesToEdges = toList . filterAndClassify reader
          go k0 k1 v = let k = k0 <> QId [k1]
                           edges = case v of
                                     UDFunction _ (Function _ ss) -> namesToEdges $ findDependenciesSeq ss
                                     BIFunction {} -> []
                                     UDMacro _ (Macro _ ss) -> namesToEdges $ findDependenciesSeq ss
                                     Module {} -> [] -- TODO Right now, modules have no dependencies because they have no load phase
                           inner = case v of
                                     Module m -> fold (Map.mapWithKey (go k) m)
                                     _ -> []
                       in map ((,) k) edges ++ inner
