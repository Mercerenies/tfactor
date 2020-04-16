{-# LANGUAGE FlexibleContexts, ConstraintKinds, RankNTypes, TemplateHaskell,
    GeneralizedNewtypeDeriving, TypeFamilies, DeriveTraversable #-}

module Factor.State.Types(EvalState(..), ReadOnlyState(ReadOnlyState), ReaderValue(..), RId,
                          Module(Module), AliasDecl(..), BuiltIn(..), BuiltInConstraints,
                          ResourceTable(..),
                          readerModule, readerNames, readerResources,
                          moduleNames, moduleAliases, moduleIsType,
                          newState, newReader, emptyModule, newResourceTable) where

import Factor.Error
import Factor.Id
import Factor.Code
import Factor.Type
import Factor.Trait.Types
import Factor.Stack(Stack)
import qualified Factor.Stack as Stack

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Lens
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Sequence(Seq)
import qualified Data.Sequence as Seq

data EvalState = EvalState {
      stateStack :: Stack Data
    } deriving (Show, Eq)

data ReadOnlyState = ReadOnlyState {
      _readerModule :: Module,
      _readerResources :: ResourceTable ReaderValue
    }

data ReaderValue = UDFunction PolyFunctionType Function -- User-defined function
                 | BIFunction PolyFunctionType (BuiltIn ()) -- Built-in function
                 | UDMacro PolyFunctionType Macro -- User-defined macro
                 | ModuleValue Module
                 | ModuleSynonym QId
                 | TraitValue Trait

data Module = Module {
      _moduleNames :: Map Id RId,
      _moduleAliases :: [AliasDecl],
      _moduleIsType :: Bool
    }

data AliasDecl = Alias Id QId
               | Open QId
                 deriving (Show, Eq)

type BuiltInConstraints m = (MonadReader ReadOnlyState m, MonadState EvalState m, MonadError FactorError m)

newtype BuiltIn a = BuiltIn { unBuiltIn :: forall m. BuiltInConstraints m => m a }

newtype ResourceTable a = ResourceTable (Seq (QId, a))
    deriving (Functor, Foldable, Traversable)

type instance Index (ResourceTable a) = Int
type instance IxValue (ResourceTable a) = a

instance Ixed (ResourceTable a) where
    ix n f (ResourceTable s) = ResourceTable <$> (ix n . _2) f s

type RId = Int

makeLenses ''ReadOnlyState
makeLenses ''Module

readerNames :: Lens' ReadOnlyState (Map Id RId)
readerNames = readerModule . moduleNames

newState :: EvalState
newState = EvalState Stack.empty

newReader :: ReadOnlyState
newReader = ReadOnlyState emptyModule newResourceTable

emptyModule :: Module
emptyModule = Module Map.empty [] False

newResourceTable :: ResourceTable a
newResourceTable = ResourceTable Seq.Empty
