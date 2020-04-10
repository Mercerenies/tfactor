{-# LANGUAGE OverloadedStrings #-}

module Factor.Test.Id where

import Factor.Id

import Test.HUnit

tests :: Test
tests = TestLabel "Factor.Test.Id" $ TestList [testShowId, testShowQId, testSplitQualified]

testShowId :: Test
testShowId = TestLabel "testShowId" $ TestList [test1, test2]
    where test1 = TestCase (show (Id "foo") @?= "foo")
          test2 = TestCase (show (Id "-") @?= "-")

testShowQId :: Test
testShowQId = TestLabel "testShowQId" $ TestList [test1, test2, test3]
    where test1 = TestCase (show (QId ["foo"]) @?= "foo")
          test2 = TestCase (show (QId ["foo", "bar"]) @?= "foo.bar")
          test3 = TestCase (show (QId ["foo", "bar", "-"]) @?= "foo.bar.-")

testSplitQualified :: Test
testSplitQualified = TestLabel "testSplitQualified" $ TestList [test1, test2]
    where test1 = TestCase (splitQualified "foo.bar.baz" @?= (QId ["foo", "bar", "baz"]))
          test2 = TestCase (splitQualified "foo-bar-baz" @?= (QId ["foo-bar-baz"]))

-- TODO Test qidName and idName once they start to diverge from the
-- show instances (after we add special syntax for escaping)
