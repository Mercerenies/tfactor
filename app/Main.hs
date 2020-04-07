{-# LANGUAGE LambdaCase, FlexibleContexts #-}

module Main where

import Factor.Parser
import Factor.Parser.Token
import Factor.Eval
import Factor.State
import Factor.Error
import Factor.Id
import Factor.StdLib
import Factor.Type.Checker

import System.Environment
import System.Exit
import Control.Monad.Except
import Control.Monad.Reader hiding (reader)

run :: FilePath -> ExceptT FactorError IO ()
run filename = do
  contents <- liftIO $ readFile filename
  contents' <- liftParseError $ parseManyTokens filename contents
  decls <- liftParseError $ parseFile filename contents'
  reader <- declsToReadOnly decls stdlibs
  _ <- runReaderT checkAllTypes reader
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
