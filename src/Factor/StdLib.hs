{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Factor.StdLib(builtins, stdlibs, stdlibModuleName, bindStdlibModule) where

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
import Control.Monad.State
import Control.Lens
import Data.Map(Map)
import qualified Data.Map as Map

-- I've written it in several places, but I'm writing it here again to
-- be sure. `functionType', like `Stack.fromList' and the other
-- similar functions, treats the first element of a list as the top,
-- which is backwards from the way they're displayed. Thus, the stack
-- types here will look backward to how they would look if written in
-- the language.

popStack1 :: (MonadState EvalState m, MonadError FactorError m) => m Data
popStack1 = popStack 1 >>= \case
            Stack [x] -> pure x
            _ -> error "Internal error in popStack1"

popStack2 :: (MonadState EvalState m, MonadError FactorError m) => m (Data, Data)
popStack2 = popStack 2 >>= \case
            Stack [x, y] -> pure (x, y)
            _ -> error "Internal error in popStack2"

popStack3 :: (MonadState EvalState m, MonadError FactorError m) => m (Data, Data, Data)
popStack3 = popStack 3 >>= \case
            Stack [x, y, z] -> pure (x, y, z)
            _ -> error "Internal error in popStack3"

_popStack4 :: (MonadState EvalState m, MonadError FactorError m) => m (Data, Data, Data, Data)
_popStack4 = popStack 4 >>= \case
            Stack [x, y, z, t] -> pure (x, y, z, t)
            _ -> error "Internal error in popStack4"

_popStack5 :: (MonadState EvalState m, MonadError FactorError m) => m (Data, Data, Data, Data, Data)
_popStack5 = popStack 5 >>= \case
            Stack [x, y, z, t, u] -> pure (x, y, z, t, u)
            _ -> error "Internal error in popStack5"

-- ( 'R Any -- 'R )
drop_ :: BuiltIn ()
drop_ = BuiltIn $ void (popStack 1)

-- ( 'R 'a -- 'R 'a 'a )
dup :: BuiltIn ()
dup = BuiltIn $ popStack1 >>= \x -> pushStack (Stack.fromList [x, x])

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
call = BuiltIn $ popStack1 >>= \case
       FunctionValue (Function _ ss) -> evalSeq ss
       _ -> throwError NotAFunction

-- ( 'S Bool ( 'S -- 'T ) ( 'S -- 'T ) -- 'T )
if_ :: BuiltIn ()
if_ = BuiltIn $ popStack3 >>= \(f, t, cond) -> do
        cond' <- assertBool cond
        Function _ t' <- assertFunction t
        Function _ f' <- assertFunction f
        evalSeq (if cond' then t' else f')

-- ( 'R Int Int -- 'R Int )
binmathop :: (Integer -> Integer -> Integer) -> BuiltIn ()
binmathop f = BuiltIn $ popStack2 >>= \(b, a) -> do
                b' <- assertInt b
                a' <- assertInt a
                pushStack (Stack.singleton (Int $ f a' b'))

-- ( 'R Int Int -- 'R Bool )
binmathcmp :: (Integer -> Integer -> Bool) -> BuiltIn ()
binmathcmp f = BuiltIn $ popStack2 >>= \(b, a) -> do
                b' <- assertInt b
                a' <- assertInt a
                pushStack (Stack.singleton (Bool $ f a' b'))

builtins :: Map Id ReaderValue
builtins = Map.fromList [
            (Id "drop", BIFunction (polyFunctionType [Id "R"] [PrimType TAny] (RestQuant $ Id "R") [] (RestQuant $ Id "R")) drop_),
            (Id "dup", BIFunction (polyFunctionType [Id "R", Id "a"] [QuantVar (Id "a")] (RestQuant $ Id "R") [QuantVar (Id "a"), QuantVar (Id "a")] (RestQuant $ Id "R")) dup),
            (Id "id", BIFunction (polyFunctionType [Id "R"] [] (RestQuant $ Id "R") [] (RestQuant $ Id "R")) id_),
            (Id "swap", BIFunction (polyFunctionType [Id "R", Id "a", Id "b"] [QuantVar (Id "b"), QuantVar (Id "a")] (RestQuant $ Id "R") [QuantVar (Id "a"), QuantVar (Id "b")] (RestQuant $ Id "R")) swap),
            (Id "call", BIFunction (polyFunctionType [Id "S", Id "T"] [FunType (functionType [] (RestQuant (Id "S")) [] (RestQuant (Id "T")))] (RestQuant $ Id "S") [] (RestQuant $ Id "T")) call),
            (Id "if", BIFunction (polyFunctionType [Id "S", Id "T"] [FunType (functionType [] (RestQuant (Id "S")) [] (RestQuant (Id "T"))), FunType (functionType [] (RestQuant (Id "S")) [] (RestQuant (Id "T"))), PrimType TBool] (RestQuant $ Id "S") [] (RestQuant $ Id "T")) if_),
            (Id "+", BIFunction (polyFunctionType [Id "R"] [PrimType TInt, PrimType TInt] (RestQuant $ Id "R") [PrimType TInt] (RestQuant $ Id "R")) (binmathop (+))),
            (Id "-", BIFunction (polyFunctionType [Id "R"] [PrimType TInt, PrimType TInt] (RestQuant $ Id "R") [PrimType TInt] (RestQuant $ Id "R")) (binmathop (-))),
            (Id "*", BIFunction (polyFunctionType [Id "R"] [PrimType TInt, PrimType TInt] (RestQuant $ Id "R") [PrimType TInt] (RestQuant $ Id "R")) (binmathop (*))),
            (Id "/", BIFunction (polyFunctionType [Id "R"] [PrimType TInt, PrimType TInt] (RestQuant $ Id "R") [PrimType TInt] (RestQuant $ Id "R")) (binmathop div)),
            (Id "=", BIFunction (polyFunctionType [Id "R"] [PrimType TInt, PrimType TInt] (RestQuant $ Id "R") [PrimType TBool] (RestQuant $ Id "R")) (binmathcmp (==))),
            (Id "!=", BIFunction (polyFunctionType [Id "R"] [PrimType TInt, PrimType TInt] (RestQuant $ Id "R") [PrimType TBool] (RestQuant $ Id "R")) (binmathcmp (/=))),
            (Id "<", BIFunction (polyFunctionType [Id "R"] [PrimType TInt, PrimType TInt] (RestQuant $ Id "R") [PrimType TBool] (RestQuant $ Id "R")) (binmathcmp (<))),
            (Id ">", BIFunction (polyFunctionType [Id "R"] [PrimType TInt, PrimType TInt] (RestQuant $ Id "R") [PrimType TBool] (RestQuant $ Id "R")) (binmathcmp (>))),
            (Id "<=", BIFunction (polyFunctionType [Id "R"] [PrimType TInt, PrimType TInt] (RestQuant $ Id "R") [PrimType TBool] (RestQuant $ Id "R")) (binmathcmp (<=))),
            (Id ">=", BIFunction (polyFunctionType [Id "R"] [PrimType TInt, PrimType TInt] (RestQuant $ Id "R") [PrimType TBool] (RestQuant $ Id "R")) (binmathcmp (>=)))
           ]

stdlibs :: ReadOnlyState
stdlibs = ReadOnlyState builtins

stdlibModuleName :: Id
stdlibModuleName = Id "Prelude"

bindStdlibModule :: ReadOnlyState -> ReadOnlyState
bindStdlibModule = over readerNames (Map.insert stdlibModuleName (Module builtins))
