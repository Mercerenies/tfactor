{-# LANGUAGE FlexibleContexts #-}

module Factor.Error where

import Factor.Id
import Factor.Type.Error

import Text.Parsec(ParseError)
import Control.Monad.Except
import Control.Arrow

data FactorError = NoSuchFunction Id
                 | StackUnderflow
                 | DuplicateFunctionDecl Id
                 | InternalError String
                 | ParseError ParseError
                 | TypeError TypeError
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
          TypeError t -> shows t

instance FromTypeError FactorError where
    fromTypeError = TypeError

liftParseError :: MonadError FactorError m => Either ParseError a -> m a
liftParseError = liftEither . left ParseError

liftTypeError :: MonadError FactorError m => Either TypeError a -> m a
liftTypeError = liftEither . left TypeError
