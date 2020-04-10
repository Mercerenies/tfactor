
module Main where

import qualified Factor.Test.Id
import qualified Factor.Test.Stack

import Test.HUnit

tests :: Test
tests = TestList [Factor.Test.Id.tests, Factor.Test.Stack.tests]

main :: IO ()
main = runTestTT tests >>= print
