{-# OPTIONS_GHC -Wno-type-defaults #-}

module Factor.Test.Util where

import Factor.Util

import Test.HUnit

tests :: Test
tests = TestLabel "Factor.Test.Util" $ TestList [testSepBy, testPadLeft]

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

-- TODO The rest of this file
