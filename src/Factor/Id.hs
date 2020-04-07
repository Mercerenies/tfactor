{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Factor.Id(Id(..), QId(..), splitQualified) where

import Factor.Util

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
