{-# LANGUAGE RankNTypes #-}

module Factor.Util(sepBy, padLeft, foldM1, insertOrUpdate, errorToMaybe, setFilterMap,
                   possibly, possibly', foldMapM, containsDuplicate, identifiersFrom, overLens,
                   allSplits) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Lens
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Maybe
import Data.Monoid

sepBy :: Foldable t => ShowS -> t ShowS -> ShowS
sepBy delim = maybe id id . foldr go Nothing
    where go x Nothing = Just x
          go x (Just y) = Just (x . delim . y)

-- The seemingly pointless `take n' here allows this function to
-- operate (and pass through harmlessly) on infinite lists.
padLeft :: Int -> a -> [a] -> [a]
padLeft n x xs = replicate (n - length (take n xs)) x ++ xs

foldM1 :: (Foldable t, Monad m) => (a -> a -> m a) -> t a -> m a
foldM1 f = fmap (maybe (error "foldM1 on empty foldable") id) . foldM go Nothing
    where go Nothing y = pure (Just y)
          go (Just x) y = Just <$> f x y

insertOrUpdate :: Ord k => (Maybe a -> a) -> k -> Map k a -> Map k a
insertOrUpdate f = Map.alter (Just . f)

errorToMaybe :: MonadError e m => m a -> m (Maybe a)
errorToMaybe a = catchError (fmap Just a) (const (pure Nothing))

setFilterMap :: Ord b => (a -> Maybe b) -> Set a -> Set b
setFilterMap f = Set.map fromJust . Set.delete Nothing . Set.map f

possibly :: Traversal' s a -> Getter s (Maybe a)
possibly t = possibly' (First . Just) t . to getFirst

possibly' :: Monoid b => (a -> b) -> Traversal' s a -> Getter s b
possibly' f t = to $ getConst . t (Const . f)

foldMapM :: (Foldable t, Monad m, Monoid b) => (a -> m b) -> t a -> m b
foldMapM f = foldM (\acc val -> fmap (acc <>) $ f val) mempty

containsDuplicate :: Ord a => [a] -> Maybe a
containsDuplicate = go . List.sort
    where go (x:y:_) | x == y = Just x
          go (_:xs) = go xs
          go [] = Nothing

-- Given input ['a', 'b'], produces ["a", "b", "aa", "ab", "ba", "bb", "aaa", "aab", ...]
identifiersFrom :: [a] -> [[a]]
identifiersFrom xs = concatMap ofLength [1..]
    where ofLength n = replicateM n xs

-- TODO If I'm feeling particularly masochistic someday, I may try to
-- generalize this type from Lens' to one of the insane type synonyms
-- buried in Control.Lens.
overLens :: MonadState s m => Lens' s a -> StateT a m b -> m b
overLens acc st = do
  s <- use acc
  (b, s') <- runStateT st s
  acc .= s'
  return b

-- In spirit, this is
--
-- allSplits xs = fmap (\n -> splitAt n xs) [0..]
--
-- but hilariously, that implementation actually works for infinite
-- lists and fails in the *finite* case. The below implementation is
-- correct for both.
allSplits :: [a] -> [([a], [a])]
allSplits [] = [([], [])]
allSplits (x:xs) = ([], (x:xs)) : fmap (_1 %~ (x :)) (allSplits xs)
