{-# LANGUAGE FlexibleContexts, ConstraintKinds, RankNTypes, TemplateHaskell,
    GeneralizedNewtypeDeriving, TypeFamilies, DeriveTraversable #-}

module Factor.State.Types(EvalState(..), ReadOnlyState(ReadOnlyState), ReaderValue(..), RId,
                          Module(Module), ParameterizedModule(..), FunctorInfo(..), ModuleDecl(..),
                          BuiltIn(..), BuiltInConstraints,
                          ResourceTable(..),
                          readerModule, readerNames, readerResources,
                          moduleNames, moduleDecls, moduleIsType,
                          newState, newReader, emptyModule, mapToModule, newResourceTable) where

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
    } deriving (Show)

data ReaderValue = UDFunction PolyFunctionType Function -- User-defined function
                 | BIFunction PolyFunctionType (BuiltIn ()) -- Built-in function
                 | UDMacro PolyFunctionType Macro -- User-defined macro
                 | ModuleValue Module
                 | TraitValue ParameterizedTrait

data Module = Module {
      _moduleNames :: Map Id RId,
      _moduleDecls :: [ModuleDecl],
      _moduleIsType :: Bool
    } deriving (Show)

data ParameterizedModule = ParameterizedModule [ModuleArg] (Map Id FunctorInfo)
                           deriving (Show)

data FunctorInfo = FunctorUDFunction PolyFunctionType Function
                 | FunctorUDMacro PolyFunctionType Macro
                 | FunctorModule (Map Id FunctorInfo)
                 | FunctorTrait ParameterizedTrait

data ModuleDecl = Alias Id QId
                | Open QId
                | AssertTrait TraitRef
                | ModuleSynonym Id QId
                | IncludeModule QId
                  deriving (Show, Eq)

type BuiltInConstraints m = (MonadReader ReadOnlyState m, MonadState EvalState m, MonadError FactorError m)

newtype BuiltIn a = BuiltIn { unBuiltIn :: forall m. BuiltInConstraints m => m a }

newtype ResourceTable a = ResourceTable (Seq (QId, a))
    deriving (Show, Functor, Foldable, Traversable)

type instance Index (ResourceTable a) = Int
type instance IxValue (ResourceTable a) = a

instance Ixed (ResourceTable a) where
    ix n f (ResourceTable s) = ResourceTable <$> (ix n . _2) f s

instance Show ReaderValue where
    showsPrec _ (UDFunction p _) = ("<UDFunction " ++) . shows p . (">" ++)
    showsPrec _ (BIFunction p _) = ("<BIFunction " ++) . shows p . (">" ++)
    showsPrec _ (UDMacro p _) = ("<UDMacro " ++) . shows p . (">" ++)
    showsPrec _ (ModuleValue m) = ("<ModuleValue " ++) . shows m . (">" ++)
    showsPrec _ (TraitValue t) = ("<TraitValue " ++) . shows t . (">" ++)

instance Show FunctorInfo where
    showsPrec _ (FunctorUDFunction p _) = ("<FunctorUDFunction " ++) . shows p . (">" ++)
    showsPrec _ (FunctorUDMacro p _) = ("<FunctorUDMacro " ++) . shows p . (">" ++)
    showsPrec _ (FunctorModule m) = ("<FunctorModule " ++) . shows m . (">" ++)
    showsPrec _ (FunctorTrait t) = ("<FunctorTrait " ++) . shows t . (">" ++)

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
emptyModule = mapToModule Map.empty

mapToModule :: Map Id RId -> Module
mapToModule m = Module m [] False

newResourceTable :: ResourceTable a
newResourceTable = ResourceTable Seq.Empty
