
module Factor.Code(Function(..), Macro(..), Data(..), Statement(..),
                   Sequence(..),
                   functionName, functionSeq, macroName, macroSeq,
                   makeRecord, recordGetField) where

import Factor.Id

import Data.Ix
import Data.Array.IArray

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
          | RecordInstance QId (Array Int Data)
            deriving (Show, Eq)

data Statement = Call QId
               | Literal Data
                 deriving (Show, Eq)

newtype Sequence = Sequence { unSequence :: [Statement] }
    deriving (Show, Eq)

functionName :: Function -> Maybe Id
functionName (Function i _) = i

functionSeq :: Function -> Sequence
functionSeq (Function _ s) = s

macroName :: Macro -> Id
macroName (Macro i _) = i

macroSeq :: Macro -> Sequence
macroSeq (Macro _ s) = s

makeRecord :: QId -> [Data] -> Data
makeRecord qid xs = RecordInstance qid $ listArray (0, length xs - 1) xs

recordGetField :: Int -> Array Int Data -> Maybe Data
recordGetField n arr
    | inRange (bounds arr) n = Just (arr ! n)
    | otherwise = Nothing
