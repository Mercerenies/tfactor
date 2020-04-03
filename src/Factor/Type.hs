
module Factor.Type where

import Factor.Util
import Factor.Id
import Factor.Stack(Stack, FromTop(..))
import qualified Factor.Stack as Stack

import Data.Function
import Data.Map(Map)
import qualified Data.Map as Map
--import Data.Set(Set)
import qualified Data.Set as Set

-- Note: Quantified variables can be specialized. Ground variables are
-- ground and only unify with identical variables.
data Type = PrimType PrimType
          | FunType FunctionType
          | GroundVar Id
          | QuantVar Id
            deriving (Eq)

data PolyFunctionType = PolyFunctionType [Id] FunctionType
                        deriving (Eq)

data FunctionType = FunctionType (Stack Type) (Stack Type)
                    deriving (Eq)

data PrimType = TInt | TAny | TNothing
                deriving (Eq, Ord, Enum)

instance Show PrimType where
    showsPrec _ x =
        case x of
          TInt -> ("Int" ++)
          TAny -> ("Any" ++)
          TNothing -> ("Nothing" ++)

instance Show Type where
    showsPrec n (PrimType t) = showsPrec n t
    showsPrec n (FunType t) = showsPrec n t
    showsPrec n (GroundVar t) = ("'" ++) . showsPrec n t
    showsPrec n (QuantVar t) = ("''" ++) . showsPrec n t

instance Show FunctionType where
    showsPrec _ (FunctionType args rets) =
        ("( " ++) . args' . (" -- " ++) . rets' . (" )" ++)
            where listOut = sepBy (" " ++) . fmap (showsPrec 10) . Stack.FromBottom
                  args' = listOut args
                  rets' = listOut rets

instance Show PolyFunctionType where
    showsPrec n (PolyFunctionType _ f) = showsPrec n f

emptyFnType :: FunctionType
emptyFnType = FunctionType Stack.empty Stack.empty

emptyPolyFnType :: PolyFunctionType
emptyPolyFnType = PolyFunctionType [] emptyFnType

-- Top of stack is to the left, as per usual
functionType :: [Type] -> [Type] -> FunctionType
functionType = FunctionType `on` Stack.fromList

polyFunctionType :: [Id] -> [Type] -> [Type] -> PolyFunctionType
polyFunctionType ids args rets = PolyFunctionType ids (functionType args rets)

liftFnType :: FunctionType -> PolyFunctionType
liftFnType = PolyFunctionType []

underlyingFnType :: PolyFunctionType -> FunctionType
underlyingFnType (PolyFunctionType _ t) = t

-- Ground, then Quant
gensub :: (Id -> Type) -> (Id -> Type) -> Type -> Type
gensub a b = go
    where go (PrimType t) = PrimType t
          go (FunType (FunctionType xs ys)) = FunType (FunctionType (fmap go xs) (fmap go ys))
          go (GroundVar v) = a v
          go (QuantVar v) = b v

substitute :: Map Id Type -> Type -> Type
substitute m = gensub GroundVar trysub
    where trysub v
              | Just t <- Map.lookup v m = t
              | otherwise = QuantVar v

substituteUntilDone :: Map Id Type -> Type -> Type
substituteUntilDone m x = let x' = substitute m x in if x == x' then x else substituteUntilDone m x'

toGround :: [Id] -> Type -> Type
toGround ids = gensub GroundVar test
    where test v
              | v `elem` ids = GroundVar v
              | otherwise = QuantVar v

toQuant :: [Id] -> Type -> Type
toQuant ids = gensub test QuantVar
    where test v
              | v `elem` ids = QuantVar v
              | otherwise = GroundVar v

allGroundVars :: Type -> [Id]
allGroundVars = Set.toList . go
    where go (PrimType {}) = Set.empty
          go (FunType (FunctionType xs ys)) = foldMap go (FromTop xs) <> foldMap go (FromTop ys)
          go (GroundVar v) = Set.singleton v
          go (QuantVar {}) = Set.empty

allQuantVars :: Type -> [Id]
allQuantVars = Set.toList . go
    where go (PrimType {}) = Set.empty
          go (FunType (FunctionType xs ys)) = foldMap go (FromTop xs) <> foldMap go (FromTop ys)
          go (GroundVar {}) = Set.empty
          go (QuantVar v) = Set.singleton v

-- Renames Quants
renameToAvoidConflicts :: (Id -> Bool) -> Type -> Type
renameToAvoidConflicts conflict t = gensub GroundVar (QuantVar . rename) t
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

newQuant :: Type -> Id
newQuant t = head $ filter (not . isConflicting) [Id "t" <> Id (show n) | n <- [0 :: Int ..]]
    where quants = allQuantVars t
          isConflicting = (`elem` quants)

-- TODO: Somewhere we need to check for cycles, so we know these
-- substitutions will terminate. Cyclic type assumptions are an error.
