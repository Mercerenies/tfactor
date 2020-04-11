
module Main where

import qualified Factor.Test.Id
import qualified Factor.Test.Stack
import qualified Factor.Test.Util
import qualified Factor.Test.StdLib

import Test.HUnit

tests :: Test
tests = TestList [Factor.Test.Id.tests, Factor.Test.Stack.tests, Factor.Test.Util.tests,
                  Factor.Test.StdLib.tests]

main :: IO ()
main = runTestTT tests >>= print
