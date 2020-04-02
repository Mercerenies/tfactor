
module Factor.Util where

sepBy :: Foldable t => ShowS -> t ShowS -> ShowS
sepBy delim = maybe id id . foldr go Nothing
    where go x Nothing = Just x
          go x (Just y) = Just (x . delim . y)

-- The seemingly pointless `take n' here allows this function to
-- operate (and pass through harmlessly) on infinite lists.
padLeft :: Int -> a -> [a] -> [a]
padLeft n x xs = replicate (n - length (take n xs)) x ++ xs
