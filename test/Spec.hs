
module Main where

import qualified Factor.Test.Id
import qualified Factor.Test.Stack
import qualified Factor.Test.Util
import qualified Factor.Test.StdLib
import qualified Factor.Test.Files
import Factor.Test.TestUtil

import Test.HUnit

coreTests :: [Test]
coreTests = [Factor.Test.Id.tests, Factor.Test.Stack.tests, Factor.Test.Util.tests]

vmTests :: SharedPrelude -> [Test]
vmTests prelude = [Factor.Test.StdLib.tests prelude, Factor.Test.Files.tests prelude]

main :: IO ()
main = do
  prelude <- newSharedPrelude
  let tests = TestList $ coreTests ++ vmTests prelude
  result <- runTestTT tests
  print result
