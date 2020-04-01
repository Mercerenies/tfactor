{-# LANGUAGE FlexibleContexts #-}

module Factor.Error where

import Factor.Id

import Text.Parsec(ParseError)
import Control.Monad.Except
import Control.Arrow

data FactorError = NoSuchFunction Id
                 | StackUnderflow
                 | DuplicateFunctionDecl Id
                 | InternalError String
                 | ParseError ParseError
                   deriving (Eq)

instance Show FactorError where
    showsPrec _ err =
        case err of
          NoSuchFunction v -> ("No such function " ++) . shows v
          StackUnderflow -> ("Stack underflow" ++)
          DuplicateFunctionDecl v -> ("Duplicate function declaration " ++) . shows v
          InternalError str ->
              ("Internal error " ++) . shows str . (" (this message should not appear)" ++)
          ParseError p -> shows p

liftParseError :: MonadError FactorError m => Either ParseError a -> m a
liftParseError = liftEither . left ParseError
