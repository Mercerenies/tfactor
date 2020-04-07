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
import qualified Data.Map as Map

-- The ID will be for better error messages later
checkTypeOf :: (MonadError FactorError m, MonadReader ReadOnlyState m) => Id -> ReaderFunction -> m ()
checkTypeOf _ (UDFunction t f) = checkDeclaredType t f
checkTypeOf _ (BIFunction _ _) = pure () -- We don't typecheck primitives.

run :: FilePath -> ExceptT FactorError IO ()
run filename = do
  contents <- liftIO $ readFile filename
  contents' <- liftParseError $ parseManyTokens filename contents
  decls <- liftParseError $ parseFile filename contents'
  reader <- declsToReadOnly decls stdlibs
  _ <- runReaderT (Map.traverseWithKey checkTypeOf $ readerFunctions reader)
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
