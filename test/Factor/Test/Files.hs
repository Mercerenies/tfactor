
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
           testFile p "std/test/modules_1.txt",
           testFile p "std/test/modules_2.txt",
           testFile p "std/test/modules_3.txt",
           testFile p "std/test/modules_4.txt",
           testFile p "std/test/modules_5.txt",
           testFile p "std/test/modules_6.txt",
           testFile p "std/test/modules_7.txt",
           testFile p "std/test/modules_8.txt",
           testFile p "std/test/modules_9.txt",
           testFile p "std/test/usertypes_1.txt",
           testFile p "std/test/usertypes_2.txt",
           testFile p "std/test/usertypes_3.txt",
           testFile p "std/test/usertypes_4.txt",
           testFile p "std/test/usertypes_5.txt",
           testFile p "std/test/usertypes_6.txt",
           testFile p "std/test/usertypes_7.txt",
           testFile p "std/test/usertypes_8.txt",
           testFile p "std/test/usertypes_9.txt",
           testFile p "std/test/usertypes_10.txt",
           testFile p "std/test/usertypes_11.txt",
           testFile p "std/test/usertypes_12.txt",
           testFile p "std/test/usertypes_13.txt",
           testFile p "std/test/usertypes_14.txt",
           testFile p "std/test/traits_1.txt",
           testFile p "std/test/traits_2.txt",
           testFile p "std/test/traits_3.txt",
           testFile p "std/test/traits_4.txt",
           testFile p "std/test/traits_5.txt",
           testFile p "std/test/traits_6.txt",
           testFile p "std/test/traits_7.txt",
           testFile p "std/test/general_1.txt"
          ]

testFile :: SharedPrelude -> FilePath -> Test
testFile p f = TestLabel f . TestCase $ expectTrueFromFile p f

