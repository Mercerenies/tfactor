{-# LANGUAGE LambdaCase, FlexibleContexts #-}

module Main where

import Factor.Parser
import Factor.Parser.Token
import Factor.Eval
import Factor.State
import Factor.State.Alias
import Factor.Error
import Factor.Id
import Factor.StdLib
import Factor.Loader

import System.Environment
import System.Exit
import Control.Monad.Except
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
  definednames <- declsToReadOnly decls emptyModule
  let newbindings = ReadOnlyState definednames
  fullbindings <- bindStdlibModule prelude newbindings
  aliases <- lookupAndOpenModule (QId [primitivesModuleName]) fullbindings Map.empty
  newbindings' <- forOf readerModule newbindings $ resolveAliasesMod aliases (QId [])
  reader'' <- bindStdlibModule prelude newbindings'
  reader''' <- loadEntities (allNames newbindings') reader''
  --_ <- runReaderT (checkAllTypes MacroPass) reader''
  -- TODO We expose some names that are dangerous and not fully loaded yet here.
  --reader''' <- forOf readerNames reader'' $ \r -> runReaderT (traverse augmentWithMacros r) reader''
  --_ <- runReaderT (checkAllTypes FunctionPass) reader'''
  (_, state) <- liftEither $ runEval (callFunction (QId [Id "main"])) reader''' newState
  liftIO $ print state

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "One command line argument expected" >> exitFailure
    (filename:_) -> runExceptT (run filename) >>= \case
                    Left err -> print err >> exitFailure
                    Right () -> pure ()
