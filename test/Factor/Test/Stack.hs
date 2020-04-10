{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Factor.Test.Stack where

import Factor.Stack

import Test.HUnit
import qualified Data.Foldable as Fold
import Prelude hiding (length, reverse)

tests :: Test
tests = TestLabel "Factor.Test.Stack" $
        TestList [testStackInstances, testStackFolds, testStackTraversals,
                  testStackConstructors, testStackModifiers, testSplitStack,
                  testStackTake, testStackHelpers]

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

testStackConstructors :: Test
testStackConstructors = TestLabel "testStackConstructors" $ TestList [
                         TestCase ((empty :: Stack Int) @?= Stack []),
                         TestCase (singleton "abcd" @?= Stack ["abcd"]),
                         TestCase (fromList [1, 2, 3] @?= Stack [1, 2, 3]),
                         TestCase ((fromList [] :: Stack Int) @?= Stack [])
                        ]

testStackModifiers :: Test
testStackModifiers = TestLabel "testStackModifiers" $ TestList [
                      TestCase (pushStack 3 empty @?= Stack [3]),
                      TestCase (pushStack "AB" (Stack ["CD", "EF"]) @?= Stack ["AB", "CD", "EF"]),
                      TestCase (appendStack (Stack [1, 2]) (Stack [3, 4]) @?= Stack [1, 2, 3, 4]),
                      TestCase (popStack (Stack [] :: Stack Int) @?= Nothing),
                      TestCase (popStack (Stack [1]) @?= Just (1, Stack [])),
                      TestCase (popStack (Stack [10, 20]) @?= Just (10, Stack [20])),
                      TestCase (popStack (Stack [100, 200, 300]) @?= Just (100, Stack [200, 300])),
                      TestCase (peekStack (Stack [] :: Stack Int) @?= Nothing),
                      TestCase (peekStack (Stack [1]) @?= Just 1),
                      TestCase (peekStack (Stack [10, 20]) @?= Just 10),
                      TestCase (peekStack (Stack [100, 200, 300]) @?= Just 100)
                     ]

testSplitStack :: Test
testSplitStack =
    TestLabel "testSplitStack" $ TestList [
        TestCase (splitStack 0 (Stack [] :: Stack Int) @?= Just (Stack [], Stack [])),
        TestCase (splitStack 0 (Stack [1, 2, 3]) @?= Just (Stack [], Stack [1, 2, 3])),
        TestCase (splitStack 5 (Stack [] :: Stack Int) @?= Nothing),
        TestCase (splitStack 5 (Stack [1, 2, 3]) @?= Nothing),
        TestCase (splitStack (-5) (Stack [1, 2, 3]) @?= Nothing),
        TestCase (splitStack 3 (Stack [1, 2, 3, 4, 5]) @?= Just (Stack [1, 2, 3], Stack [4, 5])),
        TestCase (splitStack 1 (Stack [30, 20, 10]) @?= Just (Stack [30], Stack [20, 10])),
        TestCase (splitStack 3 (Stack [1, 2, 3]) @?= Just (Stack [1, 2, 3], Stack [])),
        TestCase (splitStack 4 (Stack [1, 2, 3]) @?= Nothing)
    ]

testStackTake :: Test
testStackTake =
    TestLabel "testStackTake" $ TestList [
        TestCase (takeTop 0 (Stack [] :: Stack Int) @?= Just (Stack [])),
        TestCase (takeTop 0 (Stack [1, 2]) @?= Just (Stack [])),
        TestCase (takeTop 2 (Stack [1, 2]) @?= Just (Stack [1, 2])),
        TestCase (takeTop 3 (Stack [1, 2, 3, 4, 5]) @?= Just (Stack [1, 2, 3])),
        TestCase (takeTop 3 (Stack ["foo", "bar", "baz"]) @?= Just (Stack ["foo", "bar", "baz"])),
        TestCase (takeTop 4 (Stack ["foo", "bar", "baz"]) @?= Nothing),
        TestCase (takeTop (-1) (Stack [8, 9, 10]) @?= Nothing),
        TestCase (takeBottom 0 (Stack [] :: Stack Int) @?= Just (Stack [])),
        TestCase (takeBottom 0 (Stack [1, 2]) @?= Just (Stack [])),
        TestCase (takeBottom 2 (Stack [1, 2]) @?= Just (Stack [1, 2])),
        TestCase (takeBottom 3 (Stack [1, 2, 3, 4, 5]) @?= Just (Stack [3, 4, 5])),
        TestCase (takeBottom 3 (Stack ["foo", "bar", "baz"]) @?= Just (Stack ["foo", "bar", "baz"])),
        TestCase (takeBottom 4 (Stack ["foo", "bar", "baz"]) @?= Nothing),
        TestCase (takeBottom (-1) (Stack [8, 9, 10]) @?= Nothing)
    ]

testStackHelpers :: Test
testStackHelpers =
    TestLabel "testStackHelpers" $ TestList [
        TestCase (length (Stack []) @?= 0),
        TestCase (length (Stack [10, 20, 30]) @?= 3),
        TestCase (length (Stack ["A", "B"]) @?= 2),
        TestCase (toList (Stack [5, 6, 7, 8]) @?= [5, 6, 7, 8]),
        TestCase (reverse (Stack [5, 6, 7, 8]) @?= Stack [8, 7, 6, 5]),
        TestCase (reverse (Stack [] :: Stack Int) @?= Stack [])
    ]

-- ///// testStackZipWithM
