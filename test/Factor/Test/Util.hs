{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Factor.Test.Util where

import Factor.Util

import Test.HUnit
import Data.Functor.Identity
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Monoid
import Control.Lens

tests :: Test
tests = TestLabel "Factor.Test.Util" $ TestList [testSepBy, testPadLeft, testFoldM1,
                                                 testInsertOrUpdate, testErrorToMaybe,
                                                 testSetFilterMap, testPossibly,
                                                 testPossibly']

testSepBy :: Test
testSepBy = TestLabel "testSepBy" $ TestList [
             TestCase (sepBy ("," ++) [] "" @?= ""),
             TestCase (sepBy ("," ++) [("ABC" ++), ("DEF" ++)] "" @?= "ABC,DEF"),
             TestCase (sepBy (", " ++) [("ABC" ++), ("DEF" ++)] "" @?= "ABC, DEF"),
             TestCase (sepBy (", " ++) [("ABC" ++), ("DEF" ++), ("GHI" ++)] "" @?= "ABC, DEF, GHI"),
             TestCase (sepBy id [] "" @?= ""),
             TestCase (sepBy id [("foo" ++), ("bar" ++)] "" @?= "foobar")
            ]

testPadLeft :: Test
testPadLeft = TestLabel "testPadLeft" $ TestList [
               TestCase (padLeft 5 '0' "ABC" @?= "00ABC"),
               TestCase (padLeft 0 '0' "ABC" @?= "ABC"),
               TestCase (padLeft 0 '0' "" @?= ""),
               TestCase (padLeft 1 ' ' "" @?= " "),
               TestCase (padLeft 3 ' ' "" @?= "   "),
               TestCase (take 20 (padLeft 10 999 [1..]) @?= take 20 [1..])
              ]

testFoldM1 :: Test
testFoldM1 = TestLabel "testFoldM1" $ TestList [
              TestCase (foldM1 helper1 [1, 2, 3, 4] @?= ([2, 3, 4], 10)),
              TestCase (foldM1 helper2 [1, 2, 3, 4] @?= Identity 1) -- TODO Test empty list (??)
             ]
    where helper1 a b = ([b], a + b)
          helper2 a _ = Identity a

testInsertOrUpdate :: Test
testInsertOrUpdate =
    TestLabel "testInsertOrUpdate" $ TestList [
     TestCase (insertOrUpdate (const 100) "A" somemap @?= Map.insert "A" 100 somemap),
     TestCase (insertOrUpdate (const 100) "B" somemap @?= Map.insert "B" 100 somemap),
     TestCase (insertOrUpdate (const 100) "C" somemap @?= Map.insert "C" 100 somemap),
     TestCase (insertOrUpdate (const 100) "D" somemap @?= Map.insert "D" 100 somemap),
     TestCase (insertOrUpdate helper "A" somemap @?= Map.insert "A" 11 somemap),
     TestCase (insertOrUpdate helper "B" somemap @?= Map.insert "B" 21 somemap),
     TestCase (insertOrUpdate helper "C" somemap @?= Map.insert "C" 31 somemap),
     TestCase (insertOrUpdate helper "D" somemap @?= Map.insert "D" (-10) somemap),
     TestCase (insertOrUpdate helper "foo" Map.empty @?= Map.singleton "foo" (-10))
    ]
    where somemap = Map.fromList [("A", 10), ("B", 20), ("C", 30)]
          helper Nothing = -10
          helper (Just i) = i + 1

testErrorToMaybe :: Test
testErrorToMaybe = TestLabel "testErrorToMaybe" $ TestList [
                    TestCase (errorToMaybe Nothing @?= Just (Nothing :: Maybe Int)),
                    TestCase (errorToMaybe (Just 10) @?= Just (Just 10)),
                    TestCase (errorToMaybe (Left "error msg") @?= Right (Nothing :: Maybe Int)),
                    TestCase (errorToMaybe (Right 10 :: Either String Int) @?= Right (Just 10))
                   ]

testSetFilterMap :: Test
testSetFilterMap = TestLabel "testSetFilterMap" $ TestList [
                    TestCase (setFilterMap add1 someset @?= Set.fromList [11, 21, 31]),
                    TestCase (setFilterMap add1 emptyset @?= emptyset),
                    TestCase (setFilterMap clearall someset @?= emptyset),
                    TestCase (setFilterMap clearall emptyset @?= emptyset),
                    TestCase (setFilterMap keepnonneg someset1 @?= Set.fromList [0, 10, 20])
                   ]
    where someset = Set.fromList [10, 20, 30]
          someset1 = Set.fromList [-20, -10, 0, 10, 20]
          emptyset = Set.empty `asTypeOf` someset
          add1 = Just . (+ 1)
          clearall = const Nothing
          keepnonneg x = if x >= 0 then Just x else Nothing

testPossibly :: Test
testPossibly = TestLabel "testPossibly" $ TestList [
                TestCase (somemap^.possibly (ix "A") @?= Just 10),
                TestCase (somemap^.possibly (ix "B") @?= Just 20),
                TestCase (somemap^.possibly (ix "C") @?= Just 30),
                TestCase (somemap^.possibly (ix "D") @?= Nothing),
                TestCase ([1, 2, 3, 4]^.possibly traverse @?= Just 1),
                TestCase ([]^.possibly traverse @?= (Nothing :: Maybe Int))
               ]
    where somemap = Map.fromList [("A", 10), ("B", 20), ("C", 30)]

testPossibly' :: Test
testPossibly' = TestLabel "testPossibly'" $ TestList [
                TestCase (somemap^.possibly' Sum (ix "A") @?= Sum 10),
                TestCase (somemap^.possibly' Sum (ix "B") @?= Sum 20),
                TestCase (somemap^.possibly' Sum (ix "C") @?= Sum 30),
                TestCase (somemap^.possibly' Sum (ix "D") @?= Sum 0),
                TestCase ([1, 2, 3, 4]^.possibly' Sum traverse @?= Sum 10),
                TestCase ([]^.possibly' Sum traverse @?= Sum 0)
               ]
    where somemap = Map.fromList [("A", 10), ("B", 20), ("C", 30)]
