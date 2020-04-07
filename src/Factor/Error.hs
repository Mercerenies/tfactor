{-# LANGUAGE FlexibleContexts #-}

module Factor.Error(FactorError(..),
                    liftParseError, liftTypeError,
                    assertBool, assertFunction, assertInt) where

import Factor.Id
import Factor.Type.Error
import Factor.Type
import Factor.Code

import Text.Parsec(ParseError)
import Control.Monad.Except
import Control.Arrow

-- TODO: NotAFunction and NotAmodule are used for kind of weird things
-- right now. Need to split them into different error messages based
-- on use case (calling a non-function versus looking up a name
-- expecting a function and getting nothing) and give more parameters
-- so the error overall is better.

data FactorError = NoSuchFunction QId
                 | NoSuchModule QId
                 | StackUnderflow
                 | DuplicateDecl Id
                 | InternalError String
                 | ParseError ParseError
                 | TypeError TypeError
                 | NotAFunction
                 | NotAModule
                 | RuntimeTypeError Data Type
                 | AmbiguousName Id [QId]
                   deriving (Eq)

instance Show FactorError where
    showsPrec _ err =
        case err of
          NoSuchFunction v -> ("No such function " ++) . shows v
          NoSuchModule v -> ("No such module " ++) . shows v
          StackUnderflow -> ("Stack underflow" ++)
          DuplicateDecl v -> ("Duplicate declaration " ++) . shows v
          InternalError str ->
              ("Internal error " ++) . shows str . (" (this message should not appear)" ++)
          ParseError p -> shows p
          TypeError t -> shows t
          NotAFunction -> ("Attempt to call non-function" ++)
          NotAModule -> ("Attempt to subscript non-module" ++)
          RuntimeTypeError d t -> ("Expected value of type " ++) . shows t . (", got " ++) . shows d
          AmbiguousName n xs ->
              ("Ambiguous name " ++) . shows n . (" could refer to any of " ++) . shows xs

instance FromTypeError FactorError where
    fromTypeError = TypeError

liftParseError :: MonadError FactorError m => Either ParseError a -> m a
liftParseError = liftEither . left ParseError

liftTypeError :: MonadError FactorError m => Either TypeError a -> m a
liftTypeError = liftEither . left TypeError

assertBool :: MonadError FactorError m => Data -> m Bool
assertBool (Bool x) = pure x
assertBool v = throwError (RuntimeTypeError v (PrimType TBool))

assertFunction :: MonadError FactorError m => Data -> m Function
assertFunction (FunctionValue x) = pure x
assertFunction _ = throwError NotAFunction

assertInt :: MonadError FactorError m => Data -> m Integer
assertInt (Int x) = pure x
assertInt v = throwError (RuntimeTypeError v (PrimType TInt))
