
module Factor.Trait.Types where

import Factor.Type
import Factor.Id

data Trait = Trait [(Id, TraitInfo)]
             deriving (Show, Eq)

data ModuleArg = ModuleArg Id QId
                 deriving (Show, Eq)

data ParameterizedTrait = ParameterizedTrait [ModuleArg] Trait
                          deriving (Show, Eq)

-- Note that, for the moment, macros cannot appear as members of a
-- trait using the parser. I'll change this soon, but it's here for
-- future support. (TODO Change this)
data TraitInfo = TraitFunction PolyFunctionType
               | TraitMacro PolyFunctionType
               | TraitModule [(Id, TraitInfo)]
               | TraitInclude QId
               | TraitDemandType -- This won't isn't allowed in the parser but is used in Primitives.
                 deriving (Show, Eq)

data UnsatisfiedTrait = MissingFromTrait QId TraitInfo
                      | IncompatibleWithTrait QId TraitInfo
                        deriving (Show, Eq)

class FromUnsatisfiedTrait a where
    fromUnsatisfiedTrait :: UnsatisfiedTrait -> a

instance FromUnsatisfiedTrait UnsatisfiedTrait where
    fromUnsatisfiedTrait = id

moduleArgName :: ModuleArg -> Id
moduleArgName (ModuleArg i _) = i

moduleArgTraitName :: ModuleArg -> QId
moduleArgTraitName (ModuleArg _ q) = q
