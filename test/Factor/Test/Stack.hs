{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Factor.Test.Stack where

import Factor.Stack

import Test.HUnit
import qualified Data.Foldable as Fold

tests :: Test
tests = TestLabel "Factor.Test.Stack" $
        TestList [testStackInstances, testStackFolds, testStackTraversals]

testStackInstances :: Test
testStackInstances = TestLabel "testStackInstances" $ TestList [
                      TestCase (fmap (+ 1) (Stack [1, 2, 3, 4]) @?= Stack [2, 3, 4, 5]),
                      TestCase (Stack [1, 2] <> Stack [3, 4] @?= Stack [1, 2, 3, 4]),
                      TestCase ((mempty :: Stack Int) @?= Stack [])
                     ]

testStackFolds :: Test
testStackFolds = TestLabel "testStackFolds" $ TestList [
                  TestCase (Fold.toList (FromTop (Stack [1, 2, 3, 4])) @?= [1, 2, 3, 4]),
                  TestCase (Fold.toList (FromBottom (Stack [1, 2, 3, 4])) @?= [4, 3, 2, 1]),
                  TestCase (Fold.toList (FromTop (Stack [])) @?= ([] :: [Int])),
                  TestCase (Fold.toList (FromBottom (Stack [])) @?= ([] :: [Int]))
                 ]

testStackTraversals :: Test
testStackTraversals = TestLabel "testStackTraversals" $ TestList [
                       TestCase (traverse (\x -> ([x], x)) (FromTop (Stack [1, 2, 3, 4])) @?=
                                              ([1, 2, 3, 4], FromTop (Stack [1, 2, 3, 4]))),
                       TestCase (traverse (\x -> ([x], x)) (FromBottom (Stack [1, 2, 3, 4])) @?=
                                              ([4, 3, 2, 1], FromBottom (Stack [1, 2, 3, 4])))
                      ]
