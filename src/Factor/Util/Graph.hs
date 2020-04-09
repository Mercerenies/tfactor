
module Factor.Util.Graph(Graph(adjacencyList, incidence),
                         nullGraph, emptyGraph, fromEdges,
                         allEdges, outEdges) where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Foldable

data Graph v e = Graph {
      adjacencyList :: Map v [e],
      incidence :: e -> v -- Returns the head of the arrow (the tail
                          -- is determined by placement in the
                          -- adjacency list)
    }

instance (Show v, Show e) => Show (Graph v e) where
    showsPrec n (Graph a _) = showsPrec n a

nullGraph :: (e -> v) -> Graph v e
nullGraph = Graph Map.empty

emptyGraph :: Ord v => [v] -> (e -> v) -> Graph v e
emptyGraph vs = Graph (Map.fromList [(v, []) | v <- vs])

fromEdges :: Ord v => [v] -> [(v, e)] -> (e -> v) -> Graph v e
fromEdges vs es = Graph (Map.fromList [(v, edgesFor v) | v <- vs])
    where edgesFor v = map snd $ filter (\(v', _) -> v == v') es

allEdges :: Graph v e -> [e]
allEdges (Graph adja _) = concat $ toList adja

outEdges :: Ord v => Graph v e -> v -> [e]
outEdges (Graph adja _) v = case Map.lookup v adja of
                              Nothing -> []
                              Just es -> es
