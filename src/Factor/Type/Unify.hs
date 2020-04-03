{-# LANGUAGE FlexibleContexts, LambdaCase, ScopedTypeVariables #-}

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

import Debug.Trace

newtype AssumptionsAll = AssumptionsAll { unAssumptionsAll :: Map Id [Type] }
    deriving (Eq, Show)

newtype Assumptions = Assumptions { unAssumptions :: Map Id Type }
    deriving (Eq, Show)

instance Semigroup AssumptionsAll where
    AssumptionsAll m <> AssumptionsAll m' = AssumptionsAll $ Map.unionWith (++) m m'

instance Monoid AssumptionsAll where
    mempty = AssumptionsAll mempty

liftAssumptions :: Assumptions -> AssumptionsAll
liftAssumptions (Assumptions m) = AssumptionsAll $ fmap (\x -> [x]) m

-- This isn't a full cycle check. It deals with trivial solvable
-- cases. We need to do a proper occurs check for the cycles that
-- should err.
removeSynonyms :: Assumptions -> Assumptions
removeSynonyms (Assumptions m) =
    let classify k (QuantVar k') | k < k' = True
        classify _ _ = False
        (removals, keeps) = Map.partitionWithKey classify m
        keeps' = fmap (substituteUntilDone removals) keeps
    in Assumptions keeps'

consolidate :: MonadWriter AssumptionsAll m => AssumptionsAll -> m Assumptions
consolidate (AssumptionsAll m) = Assumptions <$> mapM (foldM intersection (PrimType TAny)) m

consolidateUntilDone :: AssumptionsAll -> Assumptions
consolidateUntilDone = removeSynonyms . go
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

union :: MonadWriter AssumptionsAll m => Type -> Type -> m Type
union a b | a == b = pure a
union (PrimType TNothing) t = pure t
union t (PrimType TNothing) = pure t
union (QuantVar a) (QuantVar b) =
    QuantVar a <$ (assume a (QuantVar b) >> assume b (QuantVar a))
union (QuantVar a) b = b <$ assume a b
union a (QuantVar b) = a <$ assume b a
union (FunType (FunctionType args1 rets1)) (FunType (FunctionType args2 rets2))
    | Stack.length rets1 - Stack.length args1 /= Stack.length rets2 - Stack.length args2 =
        pure (PrimType TAny)
    | Stack.length rets1 > Stack.length rets2 =
        union (FunType (FunctionType args2 rets2)) (FunType (FunctionType args1 rets1))
    | otherwise =
        let missing = Stack.length args2 - Stack.length args1
            extra = fromJust $ Stack.takeBottom missing args2
            args1' = args1 <> extra
            rets1' = rets1 <> extra
        in (\a r -> FunType (FunctionType a r)) <$> Stack.zipWithM intersection args1' args2 <*> Stack.zipWithM union rets1' rets2
union _ _ = pure (PrimType TAny)

intersection :: MonadWriter AssumptionsAll m => Type -> Type -> m Type
intersection a b | a == b = pure a
intersection (PrimType TAny) t = pure t
intersection t (PrimType TAny) = pure t
intersection (QuantVar a) (QuantVar b) =
    QuantVar a <$ (assume a (QuantVar b) >> assume b (QuantVar a))
intersection (QuantVar a) b = b <$ assume a b
intersection a (QuantVar b) = a <$ assume b a
intersection (FunType (FunctionType args1 rets1)) (FunType (FunctionType args2 rets2))
    | Stack.length rets1 - Stack.length args1 /= Stack.length rets2 - Stack.length args2 =
        pure (PrimType TNothing)
    | Stack.length rets1 > Stack.length rets2 =
        intersection (FunType (FunctionType args2 rets2)) (FunType (FunctionType args1 rets1))
    | otherwise =
        let missing = Stack.length args2 - Stack.length args1
            extra = fromJust $ Stack.takeBottom missing args2
            extra' = fromJust $ Stack.takeBottom missing rets2
            args2' = fromJust $ Stack.takeTop (Stack.length args2 - missing) args2
            rets2' = fromJust $ Stack.takeTop (Stack.length rets2 - missing) rets2
        in runExceptT (Stack.zipWithM isSubtypeOf extra extra') >>= \case
           Left (_ :: TypeError) -> pure (PrimType TNothing)
           Right _ ->
               (\a r -> FunType (FunctionType a r)) <$> Stack.zipWithM union args1 args2' <*> Stack.zipWithM intersection rets1 rets2'
intersection _ _ = pure (PrimType TNothing)

isFnSubtypeOf :: (FromTypeError e, MonadError e m, MonadWriter AssumptionsAll m) =>
                 FunctionType -> FunctionType -> m ()
isFnSubtypeOf = isSubtypeOf `on` FunType

isSubtypeOf :: (FromTypeError e, MonadError e m, MonadWriter AssumptionsAll m) =>
               Type -> Type -> m ()
isSubtypeOf s t = do
  let noUnify = throwError (fromTypeError $ CouldNotUnify s t)
  (u, w) <- listen (s `intersection` t)
  tell w
  let u' = substituteUntilDone (coerce $ consolidateUntilDone w) u
      s' = substituteUntilDone (coerce $ consolidateUntilDone w) s
  if u' == s' then pure () else noUnify

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

padPoly :: PolyFunctionType -> PolyFunctionType -> (PolyFunctionType, PolyFunctionType)
padPoly (PolyFunctionType x (FunctionType a b)) (PolyFunctionType y (FunctionType b' c)) =
    case Stack.length b `compare` Stack.length b' of
      GT ->
          let missing = Stack.length b - Stack.length b'
              extra = fromJust $ Stack.takeBottom missing b
              y' = y `List.union` (x `List.intersect` (concatMap allQuantVars $ Stack.toList extra))
          in (PolyFunctionType x (FunctionType a b), PolyFunctionType y' (FunctionType (b' <> extra) (c <> extra)))
      LT ->
          let missing = Stack.length b' - Stack.length b
              extra = fromJust $ Stack.takeBottom missing b'
              x' = x `List.union` (y `List.intersect` (concatMap allQuantVars $ Stack.toList extra))
          in (PolyFunctionType x' (FunctionType (a <> extra) (b <> extra)), PolyFunctionType y (FunctionType b' c))
      EQ ->
          -- They're already the correct dimensions, so pass through
          (PolyFunctionType x (FunctionType a b), PolyFunctionType y (FunctionType b' c))

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
composePFunctions f g = go resolved
    where allQuants = i `List.union` j
          (PolyFunctionType i f1, PolyFunctionType j g1) = padPoly f g
          resolved = (f1, renameToAvoidConflicts' (`elem` i) g1)
          go (f0, g0) = do
            traceShowM ("A", f0, g0)
            (h, AssumptionsAll w) <- listen (composeFunctions f0 g0)
            traceShowM ("B", h, AssumptionsAll w)
            let (univ, assum) = Map.partitionWithKey (\k _ -> k `elem` allQuants) w
            -- The variables universally quantified by the functions
            -- should be substituted. Those that are still
            -- unrecognized should be passed through to the
            -- MonadWriter instance.
            tell $ AssumptionsAll assum
            traceShowM ("C", univ)
            traceShowM ("D", fst $ runWriter (consolidate (coerce univ)))
            let h' = case substituteUntilDone (coerce consolidateUntilDone univ) (FunType h) of
                       FunType t -> t
                       _ -> error "Substitution changed type in composePFunctions"
                -- Intersect to remove any quantified variables that
                -- were instantiated by our assumptions
                ids = allQuantVars (FunType h') `List.intersect` allQuants
            return $ PolyFunctionType ids h'
