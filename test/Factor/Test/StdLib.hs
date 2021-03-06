
module Factor.Test.StdLib where

import Factor.Test.TestUtil
import Factor.State.Stack
import Factor.Stack(Stack(..))
import Factor.Code

import Test.HUnit

tests :: SharedPrelude -> Test
tests p =
    TestLabel "Factor.Test.StdLib" $ TestList $ fmap ($ p)
                  [testLiterals, testShuffles, testBranching, testCombinators]

testLiterals :: SharedPrelude -> Test
testLiterals p = TestLabel "testLiterals" $ TestList [
                  TestCase (runAndExpect p "" (EvalState (Stack []))),
                  TestCase (runAndExpect p "1 2" (EvalState (Stack [Int 2, Int 1]))),
                  TestCase (runAndExpect p "true false" (EvalState (Stack [Bool False, Bool True]))),
                  TestCase (runAndExpect p "1 :a" (EvalState (Stack [Symbol "a", Int 1]))),
                  TestCase (runAndExpect p "\"foo\"" (EvalState (Stack [String "foo"]))),
                  TestCase (runAndMatch p "1 2 3" "1 2 3")
                 ]

testShuffles :: SharedPrelude -> Test
testShuffles p = TestLabel "testShuffles" $ TestList [
                  TestCase (runAndMatch p ":abc :def :ghi drop" ":abc :def"),
                  TestCase (runAndMatch p "1 drop" ""),
                  TestCase (runAndMatch p "100 200 300 400 drop2" "100 200"),
                  TestCase (runAndMatch p "10 20 30 drop3" ""),
                  TestCase (runAndMatch p "10 20 30 40 drop3" "10"),
                  TestCase (runAndMatch p "\"A\" \"B\" \"C\" nip" "\"A\" \"C\""),
                  TestCase (runAndMatch p ":foo :bar :baz 100 nip2" ":foo 100"),
                  TestCase (runAndMatch p "1 2 3 4 5 6 7 nip3" "1 2 3 7"),
                  TestCase (runAndMatch p "10 20 dup" "10 20 20"),
                  TestCase (runAndMatch p "10 20 30 dup2" "10 20 30 20 30"),
                  TestCase (runAndMatch p "10 20 30 40 dup3" "10 20 30 40 20 30 40"),
                  TestCase (runAndMatch p ":abc :def :ghi over" ":abc :def :ghi :def"),
                  TestCase (runAndMatch p "1 2 3 4 5 over2" "1 2 3 4 5 3 4"),
                  TestCase (runAndMatch p "100 id" "100"),
                  TestCase (runAndMatch p "id 100 id 200 id id id id" "100 200"),
                  TestCase (runAndMatch p "100 200 300 swap" "100 300 200"),
                  TestCase (runAndMatch p ":a :b swap" ":b :a"),
                  TestCase (runAndMatch p "1 2 3 4 dupd" "1 2 3 3 4"),
                  TestCase (runAndMatch p "1 2 3 4 swapd" "1 3 2 4"),
                  TestCase (runAndMatch p "1 2 3 4 rot" "1 3 4 2"),
                  TestCase (runAndMatch p "1 2 3 4 unrot" "1 4 2 3"),
                  TestCase (runAndMatch p ":foo :bar :baz rot unrot" ":foo :bar :baz"),
                  TestCase (runAndMatch p ":foo :bar :baz unrot rot" ":foo :bar :baz")
                 ]

testBranching :: SharedPrelude -> Test
testBranching p = TestLabel "testBranching" $ TestList [
                   TestCase (runAndMatch p "[ 1 2 3 ] call" "1 2 3"),
                   TestCase (runAndMatch p "1 2 3 [ ] call" "1 2 3"),
                   TestCase (runAndMatch p "1 2 [ 3 ] call" "1 2 3"),
                   TestCase (runAndMatch p "1 2 3 4 [ drop 5 ] call" "1 2 3 5"),
                   TestCase (runAndMatch p "10 20 [ swap ] call" "20 10"),
                   TestCase (runAndMatch p "100 true [ dup ] [ 200 ] if" "100 100"),
                   TestCase (runAndMatch p "100 false [ dup ] [ 200 ] if" "100 200"),
                   TestCase (runAndMatch p "[ [ 1 ] call 2 ] call" "1 2")
                  ]

testCombinators :: SharedPrelude -> Test
testCombinators p = TestLabel "testCombinators" $ TestList [
                     TestCase (runAndMatch p "1 2 3 [ drop ] dip" "1 3"),
                     TestCase (runAndMatch p "1 2 3 4 [ swap ] dip" "1 3 2 4"),
                     TestCase (runAndMatch p "1 2 3 4 [ [ drop ] dip ] dip" "1 3 4"),
                     TestCase (runAndMatch p "1 2 3 4 [ 99 ] dip" "1 2 3 99 4"),
                     TestCase (runAndMatch p "1 2 3 [ drop ] dip2" "2 3"),
                     TestCase (runAndMatch p "1 2 3 4 [ swap ] dip2" "2 1 3 4"),
                     TestCase (runAndMatch p "1 2 3 4 [ 99 ] dip2" "1 2 99 3 4"),
                     TestCase (runAndMatch p "1 2 3 4 [ drop ] dip3" "2 3 4"),
                     TestCase (runAndMatch p "1 2 3 4 5 [ swap ] dip3" "2 1 3 4 5"),
                     TestCase (runAndMatch p "1 2 3 4 [ 99 ] dip3" "1 99 2 3 4"),
                     TestCase (runAndMatch p "1 2 3 4 5 [ swap ] keep" "1 2 3 5 4 5"),
                     TestCase (runAndMatch p "1 2 3 4 5 [ [ swap ] dip ] keep2" "1 2 4 3 5 4 5"),
                     TestCase (runAndMatch p "1 2 3 4 5 [ [ swap ] dip2 ] keep3" "1 3 2 4 5 3 4 5"),
                     TestCase (runAndMatch p "1 2 3 [ 10 + ] keep" "1 2 13 3"),
                     TestCase (runAndMatch p "1 2 3 [ + ] keep2" "1 5 2 3"),
                     TestCase (runAndMatch p "1 2 3 [ + + ] keep3" "6 1 2 3"),
                     TestCase (runAndMatch p "1 2 3 [ 10 + ] [ 20 + ] bi" "1 2 13 23"),
                     TestCase (runAndMatch p "1 2 3 [ + ] [ * ] bi2" "1 5 6"),
                     TestCase (runAndMatch p "1 2 3 [ + + ] [ + - ] bi3" "6 -4"),
                     TestCase (runAndMatch p "1 2 3 [ 10 + ] [ 20 + ] [ 30 + ] tri" "1 2 13 23 33"),
                     TestCase (runAndMatch p "1 2 3 [ + ] [ * ] [ - ] tri2" "1 5 6 -1"),
                     TestCase (runAndMatch p "2 3 4 [ + + ] [ + - ] [ * * ] tri3" "9 -5 24"),
                     TestCase (runAndMatch p "1 2 [ 10 + ] [ 10 - ] bi*" "11 -8"),
                     TestCase (runAndMatch p "1 2 3 4 [ + ] [ * ] bi2*" "3 12"),
                     TestCase (runAndMatch p "1 2 3 [ 10 + ] [ 20 + ] [ 30 + ] tri*" "11 22 33"),
                     TestCase (runAndMatch p "1 2 3 4 5 6 [ + ] [ * ] [ < ] tri2*" "3 12 true")
                    ]

-- TODO Test the basic integer math operators and Boolean operators
