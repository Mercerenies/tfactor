{-# LANGUAGE FlexibleContexts #-}

module Factor.StdLib(builtins, stdlibs) where

import qualified Factor.Stack as Stack
import Factor.State
import Factor.Id
import Factor.Type

import Control.Monad
import Data.Map(Map)
import qualified Data.Map as Map

-- I've written it in several places, but I'm writing it here again to
-- be sure. `functionType', like `Stack.fromList' and the other
-- similar functions, treats the first element of a list as the top,
-- which is backwards from the way they're displayed. Thus, the stack
-- types here will look backward to how they would look if written in
-- the language.

drop_ :: BuiltIn ()
drop_ = BuiltIn $ void (popStack 1)

-- This one can be written in the language as simply a no-op. But
-- sometimes explicit is better than implicit.
id_ :: BuiltIn ()
id_ = BuiltIn $ pure ()

swap :: BuiltIn ()
swap = BuiltIn $ popStack 2 >>= (pushStack . Stack.reverse)

builtins :: Map Id ReaderFunction
builtins = Map.fromList [
            (Id "drop", BIFunction (polyFunctionType [] [PrimType TAny] []) drop_),
            (Id "id", BIFunction (polyFunctionType [] [] []) id_),
            (Id "swap", BIFunction (polyFunctionType [Id "a", Id "b"] [QuantVar (Id "a"), QuantVar (Id "b")] [QuantVar (Id "b"), QuantVar (Id "a")]) swap)
           ]

stdlibs :: ReadOnlyState
stdlibs = ReadOnlyState builtins
