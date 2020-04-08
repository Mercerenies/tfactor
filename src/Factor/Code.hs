
module Factor.Code(Function(..), Macro(..), Data(..), Statement(..), Sequence(..), Declaration(..),
                   functionName, functionSeq) where

import Factor.Id
import Factor.Type

-- Functions may be named but are not necessarily so.
data Function = Function (Maybe Id) Sequence
                deriving (Show, Eq)

-- Macros, on the other hand, are always named
data Macro = Macro Id Sequence
             deriving (Show, Eq)

data Data = Int Integer
          | FunctionValue Function
          | Bool Bool
          | String String
          | Symbol String -- TODO Actually implement these things in some kind of efficient way
            deriving (Show, Eq)

data Statement = Call QId
               | Literal Data
                 deriving (Show, Eq)

newtype Sequence = Sequence { unSequence :: [Statement] }
    deriving (Show, Eq)

data Declaration = FunctionDecl PolyFunctionType Function
                 | MacroDecl PolyFunctionType Macro
                 | ModuleDecl Id [Declaration]
                   deriving (Show, Eq)

functionName :: Function -> Maybe Id
functionName (Function i _) = i

functionSeq :: Function -> Sequence
functionSeq (Function _ s) = s
