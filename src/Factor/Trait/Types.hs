
module Factor.Trait.Types where

import Factor.Type
import Factor.Id

data Trait = Trait [(Id, TraitInfo)]
             deriving (Show, Eq)

-- Note that, for the moment, macros cannot appear as members of a
-- trait using the parser. I'll change this soon, but it's here for
-- future support. (TODO Change this)
data TraitInfo = TraitFunction PolyFunctionType
               | TraitMacro PolyFunctionType
               | TraitModule [(Id, TraitInfo)]
               | TraitInclude QId
                 deriving (Show, Eq)

data UnsatisfiedTrait = MissingFromTrait QId TraitInfo
                      | IncompatibleWithTrait QId TraitInfo
                        deriving (Show, Eq)

class FromUnsatisfiedTrait a where
    fromUnsatisfiedTrait :: UnsatisfiedTrait -> a

instance FromUnsatisfiedTrait UnsatisfiedTrait where
    fromUnsatisfiedTrait = id
