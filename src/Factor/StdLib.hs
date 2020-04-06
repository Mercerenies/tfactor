{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Factor.StdLib(builtins, stdlibs) where

import Factor.Stack(Stack(..))
import qualified Factor.Stack as Stack
import Factor.State
import Factor.Id
import Factor.Type
import Factor.Error
import Factor.Code
import Factor.Eval

import Control.Monad
import Control.Monad.Except
import Data.Map(Map)
import qualified Data.Map as Map

-- I've written it in several places, but I'm writing it here again to
-- be sure. `functionType', like `Stack.fromList' and the other
-- similar functions, treats the first element of a list as the top,
-- which is backwards from the way they're displayed. Thus, the stack
-- types here will look backward to how they would look if written in
-- the language.

-- ( Any -- )
drop_ :: BuiltIn ()
drop_ = BuiltIn $ void (popStack 1)

-- This one can be written in the language as simply a no-op. But
-- sometimes explicit is better than implicit.
-- ( 'R -- 'R )
id_ :: BuiltIn ()
id_ = BuiltIn $ pure ()

-- ( 'R 'a 'b -- 'R 'b 'a )
swap :: BuiltIn ()
swap = BuiltIn $ popStack 2 >>= (pushStack . Stack.reverse)

-- ( 'S ( 'S -- 'T ) -- 'T )
call :: BuiltIn ()
call = BuiltIn $ popStack 1 >>= \case
       Stack [FunctionValue (Function _ ss)] -> evalSeq ss
       Stack [_] -> throwError NotAFunction
       _ -> error "Internal error in call (from popStack)"

-- ( 'R -- 'R Bool )
true :: BuiltIn ()
true = BuiltIn $ pushStack (Stack.singleton (Bool True))

-- ( 'R -- 'R Bool )
false :: BuiltIn ()
false = BuiltIn $ pushStack (Stack.singleton (Bool False))

-- ( 'S Bool ( 'S -- 'T ) ( 'S -- 'T ) -- 'T )
if_ :: BuiltIn ()
if_ = BuiltIn $ popStack 3 >>= \case
      Stack [f, t, cond] -> do
        cond' <- assertBool cond
        Function _ t' <- assertFunction t
        Function _ f' <- assertFunction f
        evalSeq (if cond' then t' else f')
      _ -> error "Internal error in if_ (from popStack)"

builtins :: Map Id ReaderFunction
builtins = Map.fromList [
            (Id "drop", BIFunction (polyFunctionType [Id "R"] [PrimType TAny] (RestQuant $ Id "R") [] (RestQuant $ Id "R")) drop_),
            (Id "id", BIFunction (polyFunctionType [Id "R"] [] (RestQuant $ Id "R") [] (RestQuant $ Id "R")) id_),
            (Id "swap", BIFunction (polyFunctionType [Id "R", Id "a", Id "b"] [QuantVar (Id "b"), QuantVar (Id "a")] (RestQuant $ Id "R") [QuantVar (Id "a"), QuantVar (Id "b")] (RestQuant $ Id "R")) swap),
            (Id "call", BIFunction (polyFunctionType [Id "S", Id "T"] [FunType (functionType [] (RestQuant (Id "S")) [] (RestQuant (Id "T")))] (RestQuant $ Id "S") [] (RestQuant $ Id "T")) call),
            (Id "if", BIFunction (polyFunctionType [Id "S", Id "T"] [FunType (functionType [] (RestQuant (Id "S")) [] (RestQuant (Id "T"))), FunType (functionType [] (RestQuant (Id "S")) [] (RestQuant (Id "T"))), PrimType TBool] (RestQuant $ Id "S") [] (RestQuant $ Id "T")) if_),
            (Id "true", BIFunction (polyFunctionType [Id "R"] [] (RestQuant $ Id "R") [PrimType TBool] (RestQuant $ Id "R")) true),
            (Id "false", BIFunction (polyFunctionType [Id "R"] [] (RestQuant $ Id "R") [PrimType TBool] (RestQuant $ Id "R")) false)
           ]

stdlibs :: ReadOnlyState
stdlibs = ReadOnlyState builtins
