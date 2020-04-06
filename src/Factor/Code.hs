
module Factor.Code where

import Factor.Id
import Factor.Type

-- Functions may be named but are not necessarily so.
data Function = Function (Maybe Id) Sequence
                deriving (Show, Eq)

data Data = Int Integer
          | FunctionValue Function
          | Bool Bool
            deriving (Show, Eq)

data Statement = Call Id
               | Literal Data
                 deriving (Show, Eq)

newtype Sequence = Sequence { unSequence :: [Statement] }
    deriving (Show, Eq)

data Declaration = FunctionDecl PolyFunctionType Function
                   deriving (Show, Eq)

functionName :: Function -> Maybe Id
functionName (Function i _) = i

functionSeq :: Function -> Sequence
functionSeq (Function _ s) = s
