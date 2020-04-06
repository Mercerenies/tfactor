
module Factor.Util(sepBy, padLeft, foldM1) where

import Control.Monad

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

