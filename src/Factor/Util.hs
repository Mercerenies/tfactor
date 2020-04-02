
module Factor.Util where

sepBy :: ShowS -> [ShowS] -> ShowS
sepBy delim xs =
    case xs of
      [] -> id
      (y:ys) -> y . foldr (.) id (map (delim .) ys)

-- The seemingly pointless `take n' here allows this function to
-- operate (and pass through harmlessly) on infinite lists.
padLeft :: Int -> a -> [a] -> [a]
padLeft n x xs = replicate (n - length (take n xs)) x ++ xs
