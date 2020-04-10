{-# LANGUAGE FlexibleContexts, LambdaCase, OverloadedStrings #-}

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

popStack4 :: (MonadState EvalState m, MonadError FactorError m) => m (Data, Data, Data, Data)
popStack4 = popStack 4 >>= \case
            Stack [x, y, z, t] -> pure (x, y, z, t)
            _ -> error "Internal error in popStack4"

_popStack5 :: (MonadState EvalState m, MonadError FactorError m) => m (Data, Data, Data, Data, Data)
_popStack5 = popStack 5 >>= \case
            Stack [x, y, z, t, u] -> pure (x, y, z, t, u)
            _ -> error "Internal error in popStack5"

-- ( 'R Any -- 'R )
drop_ :: BuiltIn ()
drop_ = BuiltIn $ void (popStack 1)

-- ( 'R Any Any -- 'R )
drop2 :: BuiltIn ()
drop2 = BuiltIn $ void (popStack 2)

-- ( 'R Any Any Any -- 'R )
drop3 :: BuiltIn ()
drop3 = BuiltIn $ void (popStack 3)

-- ( 'R Any 'a -- 'R 'a )
nip :: BuiltIn ()
nip = BuiltIn $ popStack2 >>= (\(x, _) -> pushStack (Stack.fromList [x]))

-- ( 'R Any Any 'a -- 'R 'a )
nip2 :: BuiltIn ()
nip2 = BuiltIn $ popStack3 >>= (\(x, _, _) -> pushStack (Stack.fromList [x]))

-- ( 'R Any Any Any 'a -- 'R 'a )
nip3 :: BuiltIn ()
nip3 = BuiltIn $ popStack4 >>= (\(x, _, _, _) -> pushStack (Stack.fromList [x]))

-- ( 'R 'a -- 'R 'a 'a )
dup :: BuiltIn ()
dup = BuiltIn $ popStack1 >>= \x -> pushStack (Stack.fromList [x, x])

-- ( 'R 'a 'b -- 'R 'a 'b 'a 'b )
dup2 :: BuiltIn ()
dup2 = BuiltIn $ popStack2 >>= \(x, y) -> pushStack (Stack.fromList [x, y, x, y])

-- ( 'R 'a 'b 'c -- 'R 'a 'b 'a 'b 'c )
dup3 :: BuiltIn ()
dup3 = BuiltIn $ popStack3 >>= \(x, y, z) -> pushStack (Stack.fromList [x, y, z, x, y, z])

-- ( 'R 'a 'b -- 'R 'a 'b 'a )
over_ :: BuiltIn ()
over_ = BuiltIn $ popStack2 >>= \(x, y) -> pushStack (Stack.fromList [y, x, y])

-- ( 'R 'a 'b 'c -- 'R 'a 'b 'c 'a 'b )
over2 :: BuiltIn ()
over2 = BuiltIn $ popStack3 >>= \(x, y, z) -> pushStack (Stack.fromList [y, z, x, y, z])

-- ( 'R 'a 'b 'c 'd -- 'R 'a 'b 'c 'd 'a 'b 'c )
over3 :: BuiltIn ()
over3 = BuiltIn $ popStack4 >>= \(x, y, z, t) -> pushStack (Stack.fromList [y, z, t, x, y, z, t])

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

polyFn :: [Type] -> Id -> [Type] -> Id -> BuiltIn () -> ReaderValue
polyFn args arg rets ret =
    let fn = functionType args (RestQuant arg) rets (RestQuant ret)
    in BIFunction (PolyFunctionType (allQuantVars $ FunType fn) fn)

builtins :: Map Id ReaderValue
builtins = Map.fromList [
            ("drop", polyFn [PrimType TAny] "R" [] "R" drop_),
            ("drop2", polyFn [PrimType TAny, PrimType TAny] "R" [] "R" drop2),
            ("drop3", polyFn [PrimType TAny, PrimType TAny, PrimType TAny] "R" [] "R" drop3),
            ("nip", polyFn [QuantVar "a", PrimType TAny] "R" [QuantVar "a"] "R" nip),
            ("nip2", polyFn [QuantVar "a", PrimType TAny, PrimType TAny] "R" [QuantVar "a"] "R" nip2),
            ("nip3", polyFn [QuantVar "a", PrimType TAny, PrimType TAny, PrimType TAny] "R" [QuantVar "a"] "R" nip3),
            ("dup", polyFn [QuantVar "a"] "R" [QuantVar "a", QuantVar "a"] "R" dup),
            ("dup2", polyFn [QuantVar "b", QuantVar "a"] "R" [QuantVar "b", QuantVar "a", QuantVar "b", QuantVar "a"] "R" dup2),
            ("dup3", polyFn [QuantVar "c", QuantVar "b", QuantVar "a"] "R" [QuantVar "c", QuantVar "b", QuantVar "a", QuantVar "c", QuantVar "b", QuantVar "a"] "R" dup3),
            ("over", polyFn [QuantVar "b", QuantVar "a"] "R" [QuantVar "a", QuantVar "b", QuantVar "a"] "R" over_),
            ("over2", polyFn [QuantVar "c", QuantVar "b", QuantVar "a"] "R" [QuantVar "b", QuantVar "a", QuantVar "c", QuantVar "b", QuantVar "a"] "R" over2),
            ("over3", polyFn [QuantVar "d", QuantVar "c", QuantVar "b", QuantVar "a"] "R" [QuantVar "c", QuantVar "b", QuantVar "a", QuantVar "d", QuantVar "c", QuantVar "b", QuantVar "a"] "R" over3),
            ("id", polyFn [] "R" [] "R" id_),
            ("swap", polyFn [QuantVar "b", QuantVar "a"] "R" [QuantVar "a", QuantVar "b"] "R" swap),
            ("call", polyFn [FunType (functionType [] (RestQuant "S") [] (RestQuant "T"))] "S" [] "T" call),
            ("if", polyFn [FunType (functionType [] (RestQuant "S") [] (RestQuant "T")), FunType (functionType [] (RestQuant "S") [] (RestQuant "T")), PrimType TBool] "S" [] "T" if_),
            ("+", polyFn [PrimType TInt, PrimType TInt] "R" [PrimType TInt] "R" $ binmathop (+)),
            ("-", polyFn [PrimType TInt, PrimType TInt] "R" [PrimType TInt] "R" $ binmathop (-)),
            ("*", polyFn [PrimType TInt, PrimType TInt] "R" [PrimType TInt] "R" $ binmathop (*)),
            ("/", polyFn [PrimType TInt, PrimType TInt] "R" [PrimType TInt] "R" $ binmathop div),
            ("=", polyFn [PrimType TInt, PrimType TInt] "R" [PrimType TBool] "R" $ binmathcmp (==)),
            ("!=", polyFn [PrimType TInt, PrimType TInt] "R" [PrimType TBool] "R" $ binmathcmp (/=)),
            ("<", polyFn [PrimType TInt, PrimType TInt] "R" [PrimType TBool] "R" $ binmathcmp (<)),
            (">", polyFn [PrimType TInt, PrimType TInt] "R" [PrimType TBool] "R" $ binmathcmp (>)),
            ("<=", polyFn [PrimType TInt, PrimType TInt] "R" [PrimType TBool] "R" $ binmathcmp (<=)),
            (">=", polyFn [PrimType TInt, PrimType TInt] "R" [PrimType TBool] "R" $ binmathcmp (>=))
           ]

stdlibs :: ReadOnlyState
stdlibs = ReadOnlyState builtins

stdlibModuleName :: Id
stdlibModuleName = Id "Prelude"

bindStdlibModule :: ReadOnlyState -> ReadOnlyState
bindStdlibModule = over readerNames (Map.insert stdlibModuleName (Module builtins))
