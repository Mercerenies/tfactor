
module Factor.Test.Files where

import Factor.Test.TestUtil

import Test.HUnit

-- Here, we keep track of the test cases which are simply files to be
-- run in the VM. Each test file contains a function called "test"
-- whose stack effect is ( -- Bool ). A return value of true from this
-- function is a success. A return value of anything else is a
-- failure. An error in the VM of any kind constitutes an error
-- according to HUnit.

tests :: SharedPrelude -> Test
tests p = TestLabel "Factor.Test.Files" $ TestList [
           testFile p "std/test/recursion_1.txt"
          ]

testFile :: SharedPrelude -> FilePath -> Test
testFile p f = TestLabel f . TestCase $ expectTrueFromFile p f
