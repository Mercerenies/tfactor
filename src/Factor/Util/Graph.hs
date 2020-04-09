{-# LANGUAGE ViewPatterns #-}

module Factor.Util.Graph(Graph(adjacencyList, incidence), Cycle(..),
                         nullGraph, emptyGraph, fromEdges,
                         allVertices, allEdges, outEdges, filterEdges, removeLoops,
                         removeVertex,
                         quotient, liftToSet, collapseVertices,
                         findCycles, topSort) where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import qualified Data.List as List

data Graph v e = Graph {
      adjacencyList :: Map v [e],
      incidence :: e -> v -- Returns the head of the arrow (the tail
                          -- is determined by placement in the
                          -- adjacency list)
    }

data Cycle v e = Cycle [v] [e]
                 deriving (Eq, Ord, Show)

-- TODO Remove this or replace it with something better; it's just for
-- debugging
instance (Show v, Show e) => Show (Graph v e) where
    showsPrec n (Graph adja _) = showsPrec n adja

instance Semigroup (Cycle v e) where
    Cycle vs es <> Cycle vs' es' = Cycle (vs <> vs') (es <> es')

instance Monoid (Cycle v e) where
    mempty = Cycle [] []

nullGraph :: (e -> v) -> Graph v e
nullGraph inc = Graph Map.empty inc

emptyGraph :: Ord v => [v] -> (e -> v) -> Graph v e
emptyGraph vs = fromEdges vs []

fromEdges :: Ord v => [v] -> [(v, e)] -> (e -> v) -> Graph v e
fromEdges vs es inc = Graph (Map.fromList [(v, edgesFor v) | v <- vs]) inc
    where edgesFor v = map snd $ filter (\(v', _) -> v == v') es

allVertices :: Graph v e -> [v]
allVertices (Graph adja _) = Map.keys adja

allEdges :: Graph v e -> [(v, e)]
allEdges (Graph adja _) = concatMap (\(v, es) -> map ((,) v) es) $ Map.toList adja

outEdges :: Ord v => Graph v e -> v -> [e]
outEdges (Graph adja _) v = case Map.lookup v adja of
                              Nothing -> []
                              Just es -> es

filterEdges :: Ord v => (v -> e -> Bool) -> Graph v e -> Graph v e
filterEdges p (Graph adja inc) = Graph adja' inc
    where adja' = Map.mapWithKey (\v es -> filter (\e -> p v e) es) adja

removeLoops :: Ord v => Graph v e -> Graph v e
removeLoops g = filterEdges p g
    where p v e = let v' = incidence g e in v /= v'

removeVertex :: Ord v => v -> Graph v e -> Graph v e
removeVertex v (Graph adja inc) = Graph adja' inc
    where adja' = fmap (filter (\e -> inc e /= v)) $ Map.delete v adja

quotient :: Ord v' => (v -> v') -> Graph v e -> Graph v' e
quotient f g = fromEdges vertices edges inc'
    where inc' = f . incidence g
          vertices = List.nub . map f $ allVertices g
          edges = map (\(v, e) -> (f v, e)) $ allEdges g

liftToSet :: Ord v => Graph v e -> Graph (Set v) e
liftToSet = quotient Set.singleton

collapseVertices :: Ord v => Set v -> Graph (Set v) e -> Graph (Set v) e
collapseVertices s = quotient go
    where go s'
              | Set.null (s `Set.intersection` s') = s'
              | otherwise = s `Set.union` s'

-- TODO This repeats a lot of work right now. We start at each vertex
-- when we really only need to do each connected component if we're
-- smart about the starting point within the component.
findCycles :: (Ord v, Ord e) => Graph v e -> [Cycle v e]
findCycles g = Set.toList $ Set.unions [search v [] [] | v <- allVertices g]
    where search v vs es
              | Just i <- v `List.elemIndex` vs =
                          -- Found a cycle
                          let vs' = take (i + 1) vs
                              es' = take i es
                          in Set.singleton (Cycle vs' es')
              | otherwise = Set.unions [search v' (v:vs) (e:es) | e <- outEdges g v
                                                                , let v' = incidence g e]

topSort :: Ord v => Graph v e -> Maybe [v]
topSort = build []
    where build acc (g @ (adjacencyList -> adja))
              | Map.null adja = Just acc
              | otherwise =
                  case List.find (\(_, es) -> null es) (Map.toList adja) of
                    Nothing -> Nothing
                    Just (v, _) -> build (v : acc) (removeVertex v g)
