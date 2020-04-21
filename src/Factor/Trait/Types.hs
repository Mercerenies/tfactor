
module Factor.Trait.Types where

import Factor.Type
import Factor.Id
import Factor.Code

import Data.Map(Map)

data Trait = Trait [(Id, TraitInfo)]
             deriving (Show, Eq)

data ModuleArg = ModuleArg Id TraitRef
                 deriving (Show, Eq)

data ParameterizedTrait = ParameterizedTrait [ModuleArg] Trait
                          deriving (Show, Eq)

data TraitRef = TraitRef QId [QId]
                deriving (Show, Eq)

data ParameterizedModule = ParameterizedModule [ModuleArg] (Map Id FunctorInfo)
                           deriving (Show, Eq)

-- Note that, for the moment, macros cannot appear as members of a
-- trait using the parser. I'll change this soon, but it's here for
-- future support. (TODO Change this)
data TraitInfo = TraitFunction PolyFunctionType
               | TraitMacro PolyFunctionType
               | TraitModule [(Id, TraitInfo)]
               | TraitInclude TraitRef
               | TraitDemandType -- This won't be allowed in the parser but is used in Primitives.
                 deriving (Show, Eq)

data FunctorInfo = FunctorUDFunction PolyFunctionType Function
                 | FunctorUDMacro PolyFunctionType Macro
                 | FunctorModule (Map Id FunctorInfo)
                 | FunctorTrait ParameterizedTrait
                 | FunctorDemandType -- This won't be allowed in the parser either.
                   deriving (Eq)

data UnsatisfiedTrait = MissingFromTrait QId TraitInfo
                      | IncompatibleWithTrait QId TraitInfo
                        deriving (Show, Eq)

class FromUnsatisfiedTrait a where
    fromUnsatisfiedTrait :: UnsatisfiedTrait -> a

instance FromUnsatisfiedTrait UnsatisfiedTrait where
    fromUnsatisfiedTrait = id

moduleArgName :: ModuleArg -> Id
moduleArgName (ModuleArg i _) = i

moduleArgTraitName :: ModuleArg -> TraitRef
moduleArgTraitName (ModuleArg _ q) = q

traitRefName :: TraitRef -> QId
traitRefName (TraitRef q _) = q

traitRefArgs :: TraitRef -> [QId]
traitRefArgs (TraitRef _ qs) = qs

instance Show FunctorInfo where
    showsPrec _ (FunctorUDFunction p _) = ("<FunctorUDFunction " ++) . shows p . (">" ++)
    showsPrec _ (FunctorUDMacro p _) = ("<FunctorUDMacro " ++) . shows p . (">" ++)
    showsPrec _ (FunctorModule m) = ("<FunctorModule " ++) . shows m . (">" ++)
    showsPrec _ (FunctorTrait t) = ("<FunctorTrait " ++) . shows t . (">" ++)
    showsPrec _ FunctorDemandType = ("<FunctorDemandType>" ++)
