{-# LANGUAGE FlexibleContexts, ConstraintKinds, RankNTypes, TemplateHaskell #-}

module Factor.State.Types(EvalState(..), ReadOnlyState(ReadOnlyState), ReaderValue(..),
                          Module(Module), AliasDecl(..), BuiltIn(..), BuiltInConstraints,
                          readerModule, readerNames, moduleNames, moduleAliases, moduleIsType,
                          newState, newReader, emptyModule) where

import Factor.Error
import Factor.Id
import Factor.Code
import Factor.Type
import Factor.Stack(Stack)
import qualified Factor.Stack as Stack

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Lens
import Data.Map(Map)
import qualified Data.Map as Map

data EvalState = EvalState {
      stateStack :: Stack Data
    } deriving (Show, Eq)

data ReadOnlyState = ReadOnlyState {
      _readerModule :: Module
    }

data ReaderValue = UDFunction PolyFunctionType Function -- User-defined function
                 | BIFunction PolyFunctionType (BuiltIn ()) -- Built-in function
                 | UDMacro PolyFunctionType Macro -- User-defined macro
                 | ModuleValue Module

data Module = Module {
      _moduleNames :: Map Id ReaderValue,
      _moduleAliases :: [AliasDecl],
      _moduleIsType :: Bool
    }

data AliasDecl = Alias Id QId
               | Open QId
                 deriving (Show, Eq)

type BuiltInConstraints m = (MonadReader ReadOnlyState m, MonadState EvalState m, MonadError FactorError m)

newtype BuiltIn a = BuiltIn { unBuiltIn :: forall m. BuiltInConstraints m => m a }

makeLenses ''ReadOnlyState
makeLenses ''Module

readerNames :: Lens' ReadOnlyState (Map Id ReaderValue)
readerNames = readerModule . moduleNames

newState :: EvalState
newState = EvalState Stack.empty

newReader :: ReadOnlyState
newReader = ReadOnlyState emptyModule

emptyModule :: Module
emptyModule = Module Map.empty [] False
