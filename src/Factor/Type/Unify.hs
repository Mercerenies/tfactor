{-# LANGUAGE FlexibleContexts #-}

module Factor.Type.Unify where

import Factor.Id
import Factor.Type
import Factor.Type.Error
import qualified Factor.Stack as Stack

import Control.Monad.Except
import Control.Monad.Writer
import Data.Function
import Data.Coerce
import Data.Maybe(fromJust)
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.List as List

newtype AssumptionsAll = AssumptionsAll { unAssumptionsAll :: Map Id [Type] }
    deriving (Eq)

newtype Assumptions = Assumptions { unAssumptions :: Map Id Type }
    deriving (Eq)

instance Semigroup AssumptionsAll where
    AssumptionsAll m <> AssumptionsAll m' = AssumptionsAll $ Map.unionWith (++) m m'

instance Monoid AssumptionsAll where
    mempty = AssumptionsAll mempty

liftAssumptions :: Assumptions -> AssumptionsAll
liftAssumptions (Assumptions m) = AssumptionsAll $ fmap (\x -> [x]) m

consolidate :: MonadWriter AssumptionsAll m => AssumptionsAll -> m Assumptions
consolidate (AssumptionsAll m) = Assumptions <$> mapM (foldM intersection (PrimType TAny)) m

consolidateUntilDone :: AssumptionsAll -> Assumptions
consolidateUntilDone = go
    where go x = case runWriter (consolidate x) of
                   (a, w) | w == mempty -> a
                          | otherwise   ->  go $ liftAssumptions a <> w

assume :: MonadWriter AssumptionsAll m => Id -> Type -> m ()
assume v t = tell (AssumptionsAll (Map.singleton v [t]))

_intersectionHandlesThisCase :: Type -> ()
_intersectionHandlesThisCase (PrimType {}) = ()
_intersectionHandlesThisCase (FunType {}) = ()
_intersectionHandlesThisCase (GroundVar {}) = ()
_intersectionHandlesThisCase (QuantVar {}) = ()

-- GroundVar and (non-TAny) PrimType are trivial as they're just an
-- equality check, which is handled at the very beginning.
intersection :: MonadWriter AssumptionsAll m => Type -> Type -> m Type
intersection a b | a == b = pure a
intersection (QuantVar a) (QuantVar b) =
    QuantVar a <$ (assume a (QuantVar b) >> assume b (QuantVar a))
intersection (QuantVar a) b = b <$ assume a b
intersection a (QuantVar b) = a <$ assume b a
intersection (PrimType TAny) t = pure t
intersection t (PrimType TAny) = pure t
intersection (FunType (FunctionType args1 rets1)) (FunType (FunctionType args2 rets2))
    | Stack.length rets1 - Stack.length args1 /= Stack.length rets2 - Stack.length args2 =
        pure (PrimType TNothing)
    | Stack.length rets1 > Stack.length rets2 =
        intersection (FunType (FunctionType args2 rets2)) (FunType (FunctionType args1 rets1))
    | otherwise = do
        let missing = Stack.length args2 - Stack.length args1
            extra = fromJust $ Stack.takeBottom missing args2
            args1' = args1 <> extra
            rets1' = rets1 <> extra
        args <- Stack.zipWithM intersection args1' args2
        rets <- Stack.zipWithM intersection rets1' rets2
        return (FunType (FunctionType args rets))
intersection _ _ = pure (PrimType TNothing)

isFnSubtypeOf :: (FromTypeError e, MonadError e m, MonadWriter AssumptionsAll m) =>
                 FunctionType -> FunctionType -> m ()
isFnSubtypeOf = isSubtypeOf `on` FunType

isSubtypeOf :: (FromTypeError e, MonadError e m, MonadWriter AssumptionsAll m) =>
               Type -> Type -> m ()
isSubtypeOf t t' = t `intersection` t' >>= \t'' -> if t'' == t then pure () else noUnify
    where noUnify = throwError (fromTypeError $ CouldNotUnify t t')

pad :: FunctionType -> FunctionType -> (FunctionType, FunctionType)
pad (FunctionType a b) (FunctionType b' c) =
    case Stack.length b `compare` Stack.length b' of
      GT ->
          let missing = Stack.length b - Stack.length b'
              extra = fromJust $ Stack.takeBottom missing b
          in (FunctionType a b, FunctionType (b' <> extra) (c <> extra))
      LT ->
          let missing = Stack.length b' - Stack.length b
              extra = fromJust $ Stack.takeBottom missing b'
          in (FunctionType (a <> extra) (b <> extra), FunctionType b' c)
      EQ ->
          -- They're already the correct dimensions, so pass through
          (FunctionType a b, FunctionType b' c)

-- Left-to-right composition, so first function happens first.
composeFunctions :: (FromTypeError e, MonadError e m, MonadWriter AssumptionsAll m) =>
                    FunctionType -> FunctionType -> m FunctionType
composeFunctions f g = go (pad f g)
    where go (FunctionType a b, FunctionType b' c)
              -- Precondition: We can assume by this point that
              -- (length b == length b').
              | Stack.length b /= Stack.length b' = error "Length precondition violated in composeFunctions"
              | otherwise =
                  zipWithM_ isSubtypeOf (Stack.toList b) (Stack.toList b') >>
                  pure (FunctionType a c)

composePFunctions :: (FromTypeError e, MonadError e m, MonadWriter AssumptionsAll m) =>
                     PolyFunctionType -> PolyFunctionType -> m PolyFunctionType
composePFunctions (PolyFunctionType i f) (PolyFunctionType j g) = go $ resolve (pad f g)
    where allQuants = i `List.union` j
          resolve (f0, g0) = (f0, renameToAvoidConflicts' (`elem` i) g0)
          go (f0, g0) = do
            (h, AssumptionsAll w) <- listen (composeFunctions f0 g0)
            let (univ, assum) = Map.partitionWithKey (\k _ -> k `elem` allQuants) w
            -- The variables universally quantified by the functions
            -- should be substituted. Those that are still
            -- unrecognized should be passed through to the
            -- MonadWriter instance.
            tell $ AssumptionsAll assum
            let h' = case substituteUntilDone (coerce consolidateUntilDone univ) (FunType h) of
                       FunType t -> t
                       _ -> error "Substitution changed type in composePFunctions"
                -- Intersect to remove any quantified variables that
                -- were instantiated by our assumptions
                ids = allQuantVars (FunType h') `List.intersect` allQuants
            return $ PolyFunctionType ids h'
