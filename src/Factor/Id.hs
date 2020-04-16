{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Factor.Id(Id(..), QId(..), splitQualified, idName, qidName, freshVar,
                 prefixes, nonemptyPrefixes) where

import Factor.Util

import qualified Data.List as List
import Data.List.Split hiding (sepBy)
import Data.String

newtype Id = Id { unId :: String }
    deriving (Eq, Ord, Semigroup, Monoid, IsString)

newtype QId = QId { unQId :: [Id] }
    deriving (Eq, Ord, Semigroup, Monoid)

instance Show Id where
    showsPrec _ (Id s) = (s ++) -- TODO Make this match the in-language syntax for identifiers.

instance Show QId where
    showsPrec _ (QId xs) = sepBy ("." ++) $ fmap shows xs

splitQualified :: Id -> QId
splitQualified (Id ss) = QId . fmap Id $ splitOn "." ss

idName :: Id -> String
idName = unId

qidName :: QId -> String
qidName (QId xs) = List.intercalate "." $ fmap idName xs

freshVar :: String -> [Id] -> Id
freshVar prefix vs = head [v | n <- [0 :: Int ..], let v = Id (prefix <> show n), v `notElem` vs]

prefixes :: QId -> [QId]
prefixes (QId xs) = init . fmap QId $ List.inits xs

nonemptyPrefixes :: QId -> [QId]
nonemptyPrefixes = tail . prefixes
