{-# LANGUAGE LambdaCase #-}

module Main where

import Factor.Parser
import Factor.Eval
import Factor.State
import Factor.Error
import Factor.Id
import Factor.Type.Checker

import System.Environment
import System.Exit
import Control.Monad.Except
import Control.Monad.Reader hiding (reader)
import qualified Data.Map as Map

main' :: IO ()
main' = do
  line <- getLine
  case parseSeq "(stdin)" line of
    Left err -> print err
    Right ss ->
        case runEval (evalSeq ss) newReader newState of
          Left err -> print err
          Right ((), state) -> print (stateStack state)
  putStrLn "Hi :)"

run :: FilePath -> ExceptT FactorError IO ()
run filename = do
  contents <- liftIO $ readFile filename
  decls <- liftParseError $ parseFile filename contents
  reader <- declsToReadOnly decls
  _ <- runReaderT (Map.traverseWithKey (\_ (t, f) -> checkDeclaredType t f) $
                      readerFunctions reader)
                  reader
  (_, state) <- liftEither $ runEval (callFunction (Id "main")) reader newState
  liftIO $ print state

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "One command line argument expected" >> exitFailure
    (filename:_) -> runExceptT (run filename) >>= \case
                    Left err -> print err >> exitFailure
                    Right () -> pure ()
