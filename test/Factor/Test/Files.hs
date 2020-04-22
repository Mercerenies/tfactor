
module Factor.Test.Files where

import Factor.Test.TestUtil

import Test.HUnit

-- Here, we keep track of the test cases which are simply files to be
-- run in the VM. Each test file contains a function called "test"
-- whose stack effect is ( -- Bool ). A return value of true from this
-- function is a success. A return value of anything else is a
-- failure. An error in the VM of any kind constitutes an error
-- according to HUnit.

-- TODO There are a few places (typechecker_1.txt, etc.) where we have
-- commented out code that "should fail". We need to turn these into
-- proper test cases in which failure is expected behavior.
-- (Basically, just go through all of the test txt files; a lot of
-- them have stuff like that)

tests :: SharedPrelude -> Test
tests p = TestLabel "Factor.Test.Files" $ TestList [
           testFile p "std/test/recursion_1.txt",
           testFile p "std/test/typechecker_1.txt",
           testFile p "std/test/usertypes_1.txt"
          ]

testFile :: SharedPrelude -> FilePath -> Test
testFile p f = TestLabel f . TestCase $ expectTrueFromFile p f