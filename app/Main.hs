{-# LANGUAGE LambdaCase, FlexibleContexts #-}

module Main where

import Factor.Parser
import Factor.Parser.Token
import Factor.Eval
import Factor.State
import Factor.State.Alias
import Factor.State.Macro
import Factor.Error
import Factor.Id
import Factor.StdLib
import Factor.Type.Checker

import System.Environment
import System.Exit
import Control.Monad.Except
import Control.Monad.Reader hiding (reader)
import Control.Lens
import qualified Data.Map as Map

-- TODO The rule for right now is that macro definitions can't
-- themselves invoke macros. As in, macro resolution literally doesn't
-- happen inside of macros. We'll loosen this restriction later.

-- TODO Also, macros only expand once. We probably want to do
-- recursive expansion.

run :: FilePath -> ExceptT FactorError IO ()
run filename = do
  contents <- liftIO $ readFile filename
  contents' <- liftParseError $ parseManyTokens filename contents
  decls <- liftParseError $ parseFile filename contents'
  definednames <- declsToReadOnly decls Map.empty
  let reader = bindStdlibModule $ ReadOnlyState definednames
  aliases <- lookupAndOpenModule (QId [stdlibModuleName]) reader Map.empty
  reader' <- forOf readerNames reader $ resolveAliasesMod aliases (QId [])
  _ <- runReaderT (checkAllTypes MacroPass) reader'
  -- TODO We expose some names that are dangerous and not fully loaded yet here.
  reader'' <- forOf readerNames reader' $ \r -> runReaderT (traverse augmentWithMacros r) reader'
  _ <- runReaderT (checkAllTypes FunctionPass) reader''
  (_, state) <- liftEither $ runEval (callFunction (QId [Id "main"])) reader'' newState
  liftIO $ print state

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "One command line argument expected" >> exitFailure
    (filename:_) -> runExceptT (run filename) >>= \case
                    Left err -> print err >> exitFailure
                    Right () -> pure ()
