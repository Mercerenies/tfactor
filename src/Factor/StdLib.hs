{-# LANGUAGE FlexibleContexts #-}

module Factor.StdLib(builtins, stdlibs) where

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

builtins :: Map Id ReaderFunction
builtins = Map.fromList [
            (Id "drop", BIFunction (functionType [PrimType TAny] []) drop_)
           ]

stdlibs :: ReadOnlyState
stdlibs = ReadOnlyState builtins
