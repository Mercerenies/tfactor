
module Factor.Util(sepBy, padLeft, foldM1, insertOrUpdate, errorToMaybe, setFilterMap) where

import Control.Monad
import Control.Monad.Except
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Maybe

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
