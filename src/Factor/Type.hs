{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module Factor.Type(Type(.., TInt, TAny, TNothing, TBool, TString, TSymbol),
                   PolyFunctionType(..), FunctionType(..),
                   StackDesc(..), RestVar(..),
                   emptyFnType, emptyPolyFnType, functionType, polyFunctionType,
                   liftFnType, quantifiedVars, underlyingFnType,
                   substitute, substituteUntilDone, substituteStack, substituteStackUntilDone,
                   substituteStack', substituteStackUntilDone',
                   toGround, toQuant, allGroundVars, allQuantVars,
                   renameToAvoidConflicts, renameToAvoidConflicts', renameToAvoidConflicts'') where

import Factor.Id
import Factor.Stack(Stack, FromTop(..))
import qualified Factor.Stack as Stack
import Factor.Names

import Data.Map(Map)
import qualified Data.Map as Map
--import Data.Set(Set)
import qualified Data.Set as Set
import Control.Monad

-- Note: Quantified variables can be specialized. Ground variables are
-- ground and only unify with identical variables.
data Type = FunType FunctionType
          | ModuleType QId
          | GroundVar Id
          | QuantVar Id
            deriving (Eq, Ord)

data PolyFunctionType = PolyFunctionType [Id] FunctionType
                        deriving (Eq)

data FunctionType = FunctionType StackDesc StackDesc
                    deriving (Eq, Ord)

data StackDesc = StackDesc (Stack Type) RestVar
                 deriving (Eq, Ord)

data RestVar = RestGround Id
             | RestQuant Id
               deriving (Eq, Ord)

pattern TInt :: Type
pattern TInt <- ModuleType ((\t -> guard (t == intType)) -> Just ())
    where TInt = ModuleType intType

pattern TAny :: Type
pattern TAny <- ModuleType ((\t -> guard (t == anyType)) -> Just ())
    where TAny = ModuleType anyType

pattern TNothing :: Type
pattern TNothing <- ModuleType ((\t -> guard (t == nothingType)) -> Just ())
    where TNothing = ModuleType nothingType

pattern TBool :: Type
pattern TBool <- ModuleType ((\t -> guard (t == boolType)) -> Just ())
    where TBool = ModuleType boolType

pattern TString :: Type
pattern TString <- ModuleType ((\t -> guard (t == stringType)) -> Just ())
    where TString = ModuleType stringType

pattern TSymbol :: Type
pattern TSymbol <- ModuleType ((\t -> guard (t == symbolType)) -> Just ())
    where TSymbol = ModuleType symbolType

instance Show RestVar where
    showsPrec n (RestGround t) = ("'" ++) . showsPrec n t
    showsPrec n (RestQuant t) = ("''" ++) . showsPrec n t

instance Show StackDesc where
    showsPrec _ (StackDesc ts r) = showsPrec 10 r . foldr (.) id (listOut ts)
        where listOut = fmap (\t -> (" " ++) . showsPrec 10 t) . Stack.FromBottom

instance Show Type where
    showsPrec n (FunType t) = showsPrec n t
    showsPrec n (ModuleType t) = showsPrec n t
    showsPrec n (GroundVar t) = ("'" ++) . showsPrec n t
    showsPrec n (QuantVar t) = ("''" ++) . showsPrec n t

instance Show FunctionType where
    showsPrec _ (FunctionType args rets) =
        ("( " ++) . showsPrec 10 args . (" -- " ++) . showsPrec 10 rets . (" )" ++)

instance Show PolyFunctionType where
    showsPrec n (PolyFunctionType _ f) = showsPrec n f

emptyFnType :: RestVar -> FunctionType
emptyFnType v = FunctionType (StackDesc Stack.empty v) (StackDesc Stack.empty v)

emptyPolyFnType :: PolyFunctionType
emptyPolyFnType = PolyFunctionType [Id "R"] $ emptyFnType (RestQuant $ Id "R")

-- Top of stack is to the left, as per usual
functionType :: [Type] -> RestVar -> [Type] -> RestVar -> FunctionType
functionType t v t' v' =
    FunctionType (StackDesc (Stack.fromList t) v) (StackDesc (Stack.fromList t') v')

polyFunctionType :: [Id] -> [Type] -> RestVar -> [Type] -> RestVar -> PolyFunctionType
polyFunctionType ids args a rets r = PolyFunctionType ids (functionType args a rets r)

liftFnType :: FunctionType -> PolyFunctionType
liftFnType = PolyFunctionType []

quantifiedVars :: PolyFunctionType -> [Id]
quantifiedVars (PolyFunctionType vs _) = vs

-- This function should be regarded as generally dangerous, as it does
-- no checks that the names being introduced do not conflict with
-- names that already exist elsewhere in the type.
underlyingFnType :: PolyFunctionType -> FunctionType
underlyingFnType (PolyFunctionType _ t) = t

-- Ground, then Quant
gensub :: (Id -> Type) -> (Id -> Type) -> Type -> Type
gensub a b = go
    where go (FunType (FunctionType (StackDesc xs x) (StackDesc ys y))) =
              FunType (FunctionType (StackDesc (fmap go xs) x) (StackDesc (fmap go ys) y))
          go (ModuleType t) = ModuleType t
          go (GroundVar v) = a v
          go (QuantVar v) = b v

substitute :: Map Id Type -> Type -> Type
substitute m = gensub GroundVar trysub
    where trysub v
              | Just t <- Map.lookup v m = t
              | otherwise = QuantVar v

substituteUntilDone :: Map Id Type -> Type -> Type
substituteUntilDone m x = let x' = substitute m x in if x == x' then x else substituteUntilDone m x'

-- Ground, then Quant
gensubstack :: (Id -> StackDesc) -> (Id -> StackDesc) -> (Type -> Type, StackDesc -> StackDesc)
gensubstack a b = (go, go')
    where go (FunType (FunctionType arg ret)) = FunType (FunctionType (go' arg) (go' ret))
          go (ModuleType t) = ModuleType t
          go (GroundVar v) = GroundVar v
          go (QuantVar v) = QuantVar v
          go' (StackDesc xs x) =
              let (f, x0) = case x of
                              RestGround x1 -> (a, x1)
                              RestQuant x1 -> (b, x1)
                  StackDesc xs' x' = f x0 in StackDesc (fmap go xs <> xs') x'

substituteStack :: Map Id StackDesc -> Type -> Type
substituteStack m = fst $ gensubstack (StackDesc mempty . RestGround) trysub
    where trysub v
              | Just t <- Map.lookup v m = t
              | otherwise = (StackDesc mempty $ RestQuant v)

substituteStackUntilDone :: Map Id StackDesc -> Type -> Type
substituteStackUntilDone m x =
    let x' = substituteStack m x in if x == x' then x else substituteStackUntilDone m x'

substituteStack' :: Map Id StackDesc -> StackDesc -> StackDesc
substituteStack' m = snd $ gensubstack (StackDesc mempty . RestGround) trysub
    where trysub v
              | Just t <- Map.lookup v m = t
              | otherwise = (StackDesc mempty $ RestQuant v)

substituteStackUntilDone' :: Map Id StackDesc -> StackDesc -> StackDesc
substituteStackUntilDone' m x =
    let x' = substituteStack' m x in if x == x' then x else substituteStackUntilDone' m x'

toGround :: [Id] -> Type -> Type
toGround ids = fst (gensubstack (StackDesc mempty . RestGround) test') . gensub GroundVar test
    where test v
              | v `elem` ids = GroundVar v
              | otherwise = QuantVar v
          test' v
              | v `elem` ids = StackDesc mempty $ RestGround v
              | otherwise = StackDesc mempty $ RestQuant v

toQuant :: [Id] -> Type -> Type
toQuant ids = gensub test QuantVar
    where test v
              | v `elem` ids = QuantVar v
              | otherwise = GroundVar v

allGroundVars :: Type -> [Id]
allGroundVars = Set.toList . go
    where go (FunType (FunctionType (StackDesc xs x) (StackDesc ys y))) =
              foldMap go (FromTop xs) <> include x <> foldMap go (FromTop ys) <> include y
          go (ModuleType _) = Set.empty
          go (GroundVar v) = Set.singleton v
          go (QuantVar {}) = Set.empty
          include (RestGround x) = Set.singleton x
          include _ = Set.empty

allQuantVars :: Type -> [Id]
allQuantVars = Set.toList . go
    where go (FunType (FunctionType (StackDesc xs x) (StackDesc ys y))) =
              foldMap go (FromTop xs) <> include x <> foldMap go (FromTop ys) <> include y
          go (ModuleType _) = Set.empty
          go (GroundVar {}) = Set.empty
          go (QuantVar v) = Set.singleton v
          include (RestQuant x) = Set.singleton x
          include _ = Set.empty

-- Renames Quants
renameToAvoidConflicts :: (Id -> Bool) -> Type -> Type
renameToAvoidConflicts conflict t =
      fst (gensubstack (StackDesc mempty . RestGround) (StackDesc mempty . RestQuant . rename)) .
      gensub GroundVar (QuantVar . rename) $ t
    where rename v
              | not (conflict v) = v
              | otherwise = head [v' | n <- [0 :: Int ..]
                                     , let v' = v <> Id (show n)
                                     , not (isConflicting v')]
          quants = allQuantVars t
          isConflicting v = conflict v || v `elem` quants

renameToAvoidConflicts' :: (Id -> Bool) -> FunctionType -> FunctionType
renameToAvoidConflicts' conflict t =
    case renameToAvoidConflicts conflict (FunType t) of
      FunType t' -> t'
      _ -> error "gensub changed shape of type in renameToAvoidConflicts'"

renameToAvoidConflicts'' :: (Id -> Bool) -> [Id] -> [Id]
renameToAvoidConflicts'' conflict t = fmap rename t
    where rename v
              | not (conflict v) = v
              | otherwise = head [v' | n <- [0 :: Int ..]
                                     , let v' = v <> Id (show n)
                                     , not (isConflicting v')]
          isConflicting v = conflict v || v `elem` t
