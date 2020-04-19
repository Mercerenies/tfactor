{-# LANGUAGE LambdaCase, FlexibleContexts #-}

module Main where

import Factor.Parser
import Factor.Parser.Token
import Factor.Eval
import Factor.State
import Factor.State.Reader
import Factor.State.Stack
import Factor.State.Alias
import Factor.State.Resource
import Factor.Error
import Factor.Id
import Factor.StdLib
import Factor.Loader
import Factor.Loader.Module
import Factor.Loader.Type

import System.Environment
import System.Exit
import Control.Monad.Except
import Control.Monad.Reader hiding (reader)
import Control.Monad.State hiding (state)
import Control.Lens
import qualified Data.Map as Map

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
  fullbindings <- bindStdlibModule prelude newbindings
  aliases <- bindDefaultAliases fullbindings Map.empty
  newbindings' <-
      runReaderT (forOf (readerResources . traverseWithQId) newbindings $ \(q, v) -> resolveAliasesResource' aliases q v) fullbindings
  reader'' <- bindStdlibModule prelude newbindings'
  reader''' <- loadModules (allNames newbindings') reader''
  reader'''' <- runReaderT (forOf (readerResources.traverseWithQId) reader''' $ \(q, v) -> resolveAliasesResource' aliases q v) reader'''
  reader''''' <- normalizeAllTypes (allNames newbindings') reader''''
  reader'''''' <- loadEntities (allNames newbindings') reader'''''
  (_, state) <- liftEither $ runEval (callFunction (QId [Id "main"])) reader'''''' newState
  liftIO $ print state

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "One command line argument expected" >> exitFailure
    (filename:_) -> runExceptT (run filename) >>= \case
                    Left err -> print err >> exitFailure
                    Right () -> pure ()
