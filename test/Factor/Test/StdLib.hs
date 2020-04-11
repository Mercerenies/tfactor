
module Factor.Test.StdLib where

import Factor.StdLib
import Factor.Parser
import Factor.Parser.Token
import Factor.State
import Factor.State.Alias
import Factor.State.Macro
import Factor.Eval
import Factor.Error(liftParseError)
import Factor.Id
import Factor.Stack(Stack(..))
--import qualified Factor.Stack as Stack
import Factor.Code

import Test.HUnit
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.RWS
import qualified Data.Map as Map

eitherToFail :: (MonadFail m, Show e) => Either e a -> m a
eitherToFail (Left e) = fail (show e)
eitherToFail (Right x) = pure x

parseAndRun :: String -> IO EvalState
parseAndRun s = runExceptT go >>= eitherToFail
    where go = do
            tokens <- liftParseError $ parseManyTokens "(test case)" s
            seq_ <- liftParseError $ parseSeq "(test case)" tokens
            let fullbindings = bindStdlibModule $ ReadOnlyState Map.empty
            aliases <- lookupAndOpenModule (QId [stdlibModuleName]) fullbindings Map.empty
            seq_' <- resolveAliasesSeq aliases seq_
            seq_'' <- runReaderT (augmentSeqWithMacros seq_') fullbindings
            ((), st, ()) <- runRWST (evalSeq seq_'') fullbindings newState
            return st

runAndExpect :: String -> EvalState -> IO ()
runAndExpect s expected = do
  actual <- parseAndRun s
  assertEqual ("Running sequence " ++ show s) expected actual

runAndMatch :: String -> String -> IO ()
runAndMatch s1 s2 = do
  a1 <- parseAndRun s1
  a2 <- parseAndRun s2
  assertBool ("Running " ++ show s1 ++ " (got " ++ show a1 ++ ") and " ++
              show s2 ++ " (got " ++ show a2 ++ ")") $ a1 == a2

tests :: Test
tests = TestLabel "Factor.Test.StdLib" $ TestList [testLiterals, testShuffles, testBranching]

testLiterals :: Test
testLiterals = TestLabel "testLiterals" $ TestList [
                TestCase (runAndExpect "" (EvalState (Stack []))),
                TestCase (runAndExpect "1 2" (EvalState (Stack [Int 2, Int 1]))),
                TestCase (runAndExpect "true false" (EvalState (Stack [Bool False, Bool True]))),
                TestCase (runAndExpect "1 :a" (EvalState (Stack [Symbol "a", Int 1]))),
                TestCase (runAndExpect "\"foo\"" (EvalState (Stack [String "foo"]))),
                TestCase (runAndMatch "1 2 3" "1 2 3")
               ]

testShuffles :: Test
testShuffles = TestLabel "testShuffles" $ TestList [
                TestCase (runAndMatch ":abc :def :ghi drop" ":abc :def"),
                TestCase (runAndMatch "1 drop" ""),
                TestCase (runAndMatch "100 200 300 400 drop2" "100 200"),
                TestCase (runAndMatch "10 20 30 drop3" ""),
                TestCase (runAndMatch "10 20 30 40 drop3" "10"),
                TestCase (runAndMatch "\"A\" \"B\" \"C\" nip" "\"A\" \"C\""),
                TestCase (runAndMatch ":foo :bar :baz 100 nip2" ":foo 100"),
                TestCase (runAndMatch "1 2 3 4 5 6 7 nip3" "1 2 3 7"),
                TestCase (runAndMatch "10 20 dup" "10 20 20"),
                TestCase (runAndMatch "10 20 30 dup2" "10 20 30 20 30"),
                TestCase (runAndMatch "10 20 30 40 dup3" "10 20 30 40 20 30 40"),
                TestCase (runAndMatch ":abc :def :ghi over" ":abc :def :ghi :def"),
                TestCase (runAndMatch "1 2 3 4 5 over2" "1 2 3 4 5 3 4"),
                TestCase (runAndMatch "1 2 3 4 5 over3" "1 2 3 4 5 2 3 4"),
                TestCase (runAndMatch "100 id" "100"),
                TestCase (runAndMatch "id 100 id 200 id id id id" "100 200"),
                TestCase (runAndMatch "100 200 300 swap" "100 300 200"),
                TestCase (runAndMatch ":a :b swap" ":b :a"),
                TestCase (runAndMatch "1 2 3 4 dupd" "1 2 3 3 4"),
                TestCase (runAndMatch "1 2 3 4 swapd" "1 3 2 4"),
                TestCase (runAndMatch "1 2 3 4 rot" "1 3 4 2"),
                TestCase (runAndMatch "1 2 3 4 unrot" "1 4 2 3"),
                TestCase (runAndMatch ":foo :bar :baz rot unrot" ":foo :bar :baz"),
                TestCase (runAndMatch ":foo :bar :baz unrot rot" ":foo :bar :baz")
               ]

testBranching :: Test
testBranching = TestLabel "testBranching" $ TestList [
                 TestCase (runAndMatch "[ 1 2 3 ] call" "1 2 3"),
                 TestCase (runAndMatch "1 2 3 [ ] call" "1 2 3"),
                 TestCase (runAndMatch "1 2 [ 3 ] call" "1 2 3"),
                 TestCase (runAndMatch "1 2 3 4 [ drop 5 ] call" "1 2 3 5"),
                 TestCase (runAndMatch "10 20 [ swap ] call" "20 10"),
                 TestCase (runAndMatch "100 true [ dup ] [ 200 ] if" "100 100"),
                 TestCase (runAndMatch "100 false [ dup ] [ 200 ] if" "100 200"),
                 TestCase (runAndMatch "[ [ 1 ] call 2 ] call" "1 2")
                ]

-- TODO Test the basic integer math operators
