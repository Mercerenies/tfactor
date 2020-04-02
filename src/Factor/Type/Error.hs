
module Factor.Type.Error where

import Factor.Type

data TypeError = CouldNotUnify Type Type
                 deriving (Eq)

instance Show TypeError where
    showsPrec _ err =
        case err of
          CouldNotUnify t t' -> ("Could not unify " ++) . shows t . (" and " ++) . shows t'

-- I don't want to talk about it.
class FromTypeError a where
    fromTypeError :: TypeError -> a

instance FromTypeError TypeError where
    fromTypeError = id
