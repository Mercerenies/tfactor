
module Factor.Test.TestUtil where

import Factor.StdLib
import Factor.State.Stack
import Factor.State
import Factor.State.Alias
import Factor.State.Macro
import Factor.State.Resource
import Factor.Type.Checker
import Factor.Eval
import Factor.Parser
import Factor.Parser.Token
import Factor.Error hiding (assertBool)
import Factor.Id
import Factor.Manager
import Factor.Code
import qualified Factor.Stack as Stack

import Test.HUnit
import Data.IORef
import qualified Data.Map as Map
import Control.Exception
import Control.Concurrent.QSem
import Control.Monad.Except
import Control.Monad.RWS hiding (reader, state)
import Control.Monad.Reader hiding (reader)
import Control.Monad.State hiding (state)

-- This is a helper file which is designed to ensure that we can share
-- the read-only portions of the VM (such as the Prelude and
-- Primitives modules) across test cases. It provides an IORef guarded
-- by a semaphore to control access.

data SharedPrelude = SharedPrelude (IORef (Maybe Prelude)) QSem

newSharedPrelude :: IO SharedPrelude
newSharedPrelude = SharedPrelude <$> newIORef Nothing <*> newQSem 1

getSharedPrelude :: SharedPrelude -> IO Prelude
getSharedPrelude (SharedPrelude ref sem) =
    bracket_ (waitQSem sem) (signalQSem sem) $ do
      value <- readIORef ref
      case value of
        Just x -> return x
        Nothing -> do
                   value' <- runExceptT loadPrelude >>= either (fail . show) pure
                   writeIORef ref (Just value')
                   return value'

eitherToFail :: (MonadFail m, Show e) => Either e a -> m a
eitherToFail (Left e) = fail (show e)
eitherToFail (Right x) = pure x

parseAndRun :: SharedPrelude -> String -> IO EvalState
parseAndRun shared s = runExceptT go >>= eitherToFail
    where go = do
            prelude <- liftIO $ getSharedPrelude shared
            tokens <- liftParseError $ parseManyTokens "(test case)" s
            seq_ <- liftParseError $ parseSeq "(test case)" tokens
            fullbindings <- bindStdlibModule prelude newReader
            aliases <- bindDefaultAliases fullbindings Map.empty
            seq_' <- resolveAliasesSeq aliases seq_

            -- The typechecks are pretty weak here, since we don't
            -- have any declared type to compare them to, but we can
            -- at least check that there *is* a well-defined type for
            -- them.
            seq_'' <- flip runReaderT fullbindings $ do
                        _ <- checkHasDefinedType MacroPass seq_'
                        seq_'' <- augmentSeqWithMacros seq_'
                        _ <- checkHasDefinedType FunctionPass seq_''
                        return seq_''

            ((), st, ()) <- runRWST (evalSeq seq_'') fullbindings newState
            return st

runAndExpect :: SharedPrelude -> String -> EvalState -> IO ()
runAndExpect shared s expected = do
  actual <- parseAndRun shared s
  assertEqual ("Running sequence " ++ show s) expected actual

runAndMatch :: SharedPrelude -> String -> String -> IO ()
runAndMatch shared s1 s2 = do
  a1 <- parseAndRun shared s1
  a2 <- parseAndRun shared s2
  assertBool ("Running " ++ show s1 ++ " (got " ++ show a1 ++ ") and " ++
              show s2 ++ " (got " ++ show a2 ++ ")") $ a1 == a2

compileAndRunTest :: SharedPrelude -> String -> IO EvalState
compileAndRunTest shared s = runExceptT go >>= eitherToFail
    where go = do
            prelude <- liftIO $ getSharedPrelude shared
            decls <- liftParseError (parseManyTokens "(test case)" s >>= parseFile "(test case)")
            newbindings <- flip evalStateT newResourceTable $ do
                             definednames <- declsToReadOnly (QId []) decls emptyModule
                             resourcetable <- get
                             return $ ReadOnlyState definednames resourcetable
            reader <- fullyLoadBindings (bindStdlibModule prelude) newbindings
            (_, state) <- liftEither $ runEval (callFunction (QId [Id "test"])) reader newState
            return state

expectTrue :: SharedPrelude -> String -> String -> IO ()
expectTrue shared prefix s = do
  state <- compileAndRunTest shared s
  assertEqual prefix state (EvalState (Stack.singleton (Bool True)))

expectTrueFromFile :: SharedPrelude -> FilePath -> IO ()
expectTrueFromFile shared filename = do
  contents <- readFile filename
  expectTrue shared ("Running " ++ filename) contents
