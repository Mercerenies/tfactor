{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Factor.Id(Id(..), QId(..), splitQualified, idName, qidName) where

import Factor.Util

import qualified Data.List as List
import Data.List.Split hiding (sepBy)

newtype Id = Id { unId :: String }
    deriving (Eq, Ord, Semigroup, Monoid)

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
