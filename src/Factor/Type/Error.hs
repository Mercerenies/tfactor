
module Factor.Type.Error where

import Factor.Type
import Factor.Id

data TypeError = CouldNotUnify Type Type
               | CouldNotUnifyStack StackDesc StackDesc
               | OccursCheck [(Id, Either StackDesc Type)]
                 deriving (Eq)

instance Show TypeError where
    showsPrec _ err =
        case err of
          CouldNotUnify t t' -> ("Could not unify " ++) . shows t . (" and " ++) . shows t'
          CouldNotUnifyStack t t' -> ("Could not unify " ++) . shows t . (" and " ++) . shows t'
          OccursCheck i -> ("Occurs check " ++) . shows i

-- I don't want to talk about it.
class FromTypeError a where
    fromTypeError :: TypeError -> a

instance FromTypeError TypeError where
    fromTypeError = id
