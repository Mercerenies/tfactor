{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Factor.Stack where

import Data.Monoid
import Control.Applicative.Backwards

-- I'm tired of forgetting which side of my "stack" is the top, so I'm
-- making an ADT for it.

-- To be clear, the LEFT side of this is the top. But the ADT will
-- take care of that.
newtype Stack a = Stack { unStack :: [a] }
    deriving (Show, Read, Eq, Functor, Semigroup, Monoid)

newtype FromTop a = FromTop (Stack a)
    deriving (Show, Read, Eq, Functor)

newtype FromBottom a = FromBottom (Stack a)
    deriving (Show, Read, Eq, Functor)

instance Foldable FromTop where
    foldMap f (FromTop (Stack xs)) = foldMap f xs
    foldr f x (FromTop (Stack xs)) = foldr f x xs

instance Traversable FromTop where
    traverse f (FromTop (Stack xs)) = FromTop . Stack <$> traverse f xs

instance Foldable FromBottom where
    foldMap f (FromBottom (Stack xs)) = getDual $ foldMap (Dual . f) xs

instance Traversable FromBottom where
    traverse f (FromBottom (Stack xs)) =
        fmap (FromBottom . Stack) . forwards $ traverse (Backwards . f) xs

empty :: Stack a
empty = Stack []

singleton :: a -> Stack a
singleton a = Stack [a]

pushStack :: a -> Stack a -> Stack a
pushStack x (Stack xs) = Stack (x:xs)

-- Appends the first stack to the top of the second.
appendStack :: Stack a -> Stack a -> Stack a
appendStack (Stack xs) (Stack ys) = Stack (xs ++ ys)

popStack :: Stack a -> Maybe (a, Stack a)
popStack (Stack []) = Nothing
popStack (Stack (x:xs)) = Just (x, Stack xs)

peekStack :: Stack a -> Maybe a
peekStack = fmap fst . popStack

splitStack :: Int -> Stack a -> Maybe (Stack a, Stack a)
splitStack n (Stack xs)
    | length xs < n = Nothing
    | otherwise     = Just (Stack $ take n xs, Stack $ drop n xs)
