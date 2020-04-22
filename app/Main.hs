{-# LANGUAGE LambdaCase, FlexibleContexts #-}

module Main where

import Factor.Parser
import Factor.Parser.Token
import Factor.Eval
import Factor.State
import Factor.State.Stack
import Factor.State.Resource
import Factor.Error
import Factor.Id
import Factor.StdLib
import Factor.Manager

import System.Environment
import System.Exit
import Control.Monad.Except
import Control.Monad.State hiding (state)

-- TODO The rule for right now is that macro definitions can't
-- themselves invoke macros. As in, macro resolution literally doesn't
-- happen inside of macros. We'll loosen this restriction later.

-- TODO Also, macros only expand once. We probably want to do
-- recursive expansion.

-- TODO There's some awkwardness in macro types with symbol literals
-- vs symbols that represent function calls. Ideally, I'd like to make
-- literals all fall under some umbrella type, so we can distinguish
-- at the type level.

-- TODO Some form of "quoted" functions that are always treated as
-- data and don't contribute to recursion checks.

run :: FilePath -> ExceptT FactorError IO ()
run filename = do
  prelude <- loadPrelude
  contents <- liftIO $ readFile filename
  contents' <- liftParseError $ parseManyTokens filename contents
  decls <- liftParseError $ parseFile filename contents'
  newbindings <- flip evalStateT newResourceTable $ do
      definednames <- declsToReadOnly (QId []) decls emptyModule
      resourcetable <- get
      return $ ReadOnlyState definednames resourcetable
  reader <- fullyLoadBindings (bindStdlibModule prelude) newbindings
  (_, state) <- liftEither $ runEval (callFunction (QId [Id "main"])) reader newState
  liftIO $ print state

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "One command line argument expected" >> exitFailure
    (filename:_) -> runExceptT (run filename) >>= \case
                    Left err -> print err >> exitFailure
                    Right () -> pure ()
