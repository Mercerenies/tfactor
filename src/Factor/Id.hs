
module Factor.Id(Id(..)) where

newtype Id = Id { unId :: String }
    deriving (Eq, Ord)

instance Show Id where
    showsPrec _ (Id s) = (s ++) -- TODO Make this match the in-language syntax for identifiers.
