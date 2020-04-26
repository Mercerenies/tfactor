{-# LANGUAGE FlexibleContexts #-}

module Factor.Error(FactorError(..),
                    liftParseError, liftTypeError,
                    assertBool, assertFunction, assertInt, assertString, assertRecord) where

import Factor.Id
import Factor.Type.Error
import Factor.Type
import Factor.Code
import Factor.Trait.Types

import Text.Parsec(ParseError)
import Control.Monad.Except
import Control.Arrow hiding (arr)
import Data.Array.IArray

-- TODO: NotAFunction and NotAmodule are used for kind of weird things
-- right now. Need to split them into different error messages based
-- on use case (calling a non-function versus looking up a name
-- expecting a function and getting nothing) and give more parameters
-- so the error overall is better.

data FactorError = NoSuchFunction QId
                 | NoSuchModule QId
                 | NoSuchType QId
                 | NoSuchTrait QId
                 | StackUnderflow
                 | DuplicateDecl Id
                 | InternalError String
                 | ParseError ParseError
                 | TypeError TypeError
                 | NotAFunction
                 | NotAModule
                 | RuntimeTypeError Data Type
                 | RuntimeError String -- Generic error if nothing else really applies.
                 | AmbiguousName Id [QId]
                 | MacroRecursionLimit Sequence
                 | LoadCycle [QId]
                 | TraitError UnsatisfiedTrait
                 | TraitArgError QId Int Int
                 | FunctorArgError QId Int Int
                   deriving (Eq)

instance Show FactorError where
    showsPrec _ err =
        case err of
          NoSuchFunction v -> ("No such function " ++) . shows v
          NoSuchModule v -> ("No such module " ++) . shows v
          NoSuchType v -> ("No such type " ++) . shows v
          NoSuchTrait v -> ("No such trait " ++) . shows v
          StackUnderflow -> ("Stack underflow" ++)
          DuplicateDecl v -> ("Duplicate declaration " ++) . shows v
          InternalError str ->
              ("Internal error " ++) . shows str . (" (this message should not appear)" ++)
          ParseError p -> shows p
          TypeError t -> shows t
          NotAFunction -> ("Attempt to call non-function" ++)
          NotAModule -> ("Attempt to subscript non-module" ++)
          RuntimeTypeError d t -> ("Expected value of type " ++) . shows t . (", got " ++) . shows d
          RuntimeError e -> ("Runtime error " ++) . shows e
          AmbiguousName n xs ->
              ("Ambiguous name " ++) . shows n . (" could refer to any of " ++) . shows xs
          MacroRecursionLimit _ ->
              ("Macro recursion limit" ++) -- TODO We don't use the argument right now
          LoadCycle ids ->
              ("Cyclic load error " ++) . shows ids
          TraitError (MissingFromTrait qid info) ->
              ("Trait requires " ++) . shows info . (" at " ++) . shows qid . (" (missing)" ++)
          TraitError (IncompatibleWithTrait qid info) ->
              ("Trait requires " ++) . shows info . (" at " ++) . shows qid . (" (type is incompatible)" ++)
          TraitArgError t expected actual ->
              ("Expected " ++) . shows expected . (" arguments to trait " ++) . shows t .
              (", got " ++) . shows actual
          FunctorArgError t expected actual ->
              ("Expected " ++) . shows expected . (" arguments to functor " ++) . shows t .
              (", got " ++) . shows actual

instance FromTypeError FactorError where
    fromTypeError = TypeError

instance FromUnsatisfiedTrait FactorError where
    fromUnsatisfiedTrait = TraitError

liftParseError :: MonadError FactorError m => Either ParseError a -> m a
liftParseError = liftEither . left ParseError

liftTypeError :: MonadError FactorError m => Either TypeError a -> m a
liftTypeError = liftEither . left TypeError

assertBool :: MonadError FactorError m => Data -> m Bool
assertBool (Bool x) = pure x
assertBool v = throwError (RuntimeTypeError v TBool)

assertFunction :: MonadError FactorError m => Data -> m Function
assertFunction (FunctionValue x) = pure x
assertFunction _ = throwError NotAFunction

assertInt :: MonadError FactorError m => Data -> m Integer
assertInt (Int x) = pure x
assertInt v = throwError (RuntimeTypeError v TInt)

assertString :: MonadError FactorError m => Data -> m String
assertString (String x) = pure x
assertString v = throwError (RuntimeTypeError v TString)

assertRecord :: MonadError FactorError m => Data -> m (QId, Int, Array Int Data)
assertRecord (RecordInstance qid n arr) = pure (qid, n, arr)
assertRecord v = throwError (RuntimeError $ "expecting record, got " ++ show v)
