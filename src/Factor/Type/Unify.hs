{-# LANGUAGE FlexibleContexts, LambdaCase, ScopedTypeVariables #-}

module Factor.Type.Unify where

import Factor.Util
import Factor.Id
import Factor.Type
import Factor.Type.Error
import Factor.Stack(Stack(..))
import qualified Factor.Stack as Stack

import Control.Monad.Except
import Control.Monad.Writer
import Control.Applicative
import Data.Function
import Data.Maybe(fromJust)
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.List as List

data AssumptionsAll = AssumptionsAll (Map Id [Type]) (Map Id [StackDesc])
    deriving (Eq, Show)

data Assumptions = Assumptions (Map Id Type) (Map Id StackDesc)
    deriving (Eq, Show)

instance Semigroup AssumptionsAll where
    AssumptionsAll m0 m1 <> AssumptionsAll m0' m1' =
        AssumptionsAll (Map.unionWith (++) m0 m0') (Map.unionWith (++) m1 m1')

instance Monoid AssumptionsAll where
    mempty = AssumptionsAll mempty mempty

liftAssumptions :: Assumptions -> AssumptionsAll
liftAssumptions (Assumptions m m') = AssumptionsAll (fmap (\x -> [x]) m) (fmap (\x -> [x]) m')

-- This isn't a full cycle check. It deals with trivial solvable
-- cases. We need to do a proper occurs check for the cycles that
-- should err.
removeSynonyms :: Assumptions -> Assumptions
removeSynonyms (Assumptions m0 m1) = Assumptions (removeSynonyms0 m0) (removeSynonyms1 m1)
    where removeSynonyms0 m =
              let classify k (QuantVar k') | k < k' = True
                  classify _ _ = False
                  (removals, keeps) = Map.partitionWithKey classify m
                  keeps' = fmap (substituteUntilDone removals) keeps
              in keeps'
          removeSynonyms1 :: Map Id StackDesc -> Map Id StackDesc
          removeSynonyms1 m =
              let classify k (StackDesc (Stack []) (RestQuant k')) | k < k' = True
                  classify _ _ = False
                  (removals, keeps) = Map.partitionWithKey classify m
                  keeps' = fmap (substituteStackUntilDone' removals) keeps
              in keeps'

consolidate :: (FromTypeError e, MonadError e m, MonadWriter AssumptionsAll m) =>
               AssumptionsAll -> m Assumptions
consolidate (AssumptionsAll m0 m1) =
    Assumptions <$>
        mapM (foldM1 intersection) m0 <*>
        mapM (foldM1 intersectionStack) m1

consolidateUntilDone :: (FromTypeError e, MonadError e m) => AssumptionsAll -> m Assumptions
consolidateUntilDone = fmap removeSynonyms . go
    where go x = runWriterT (consolidate x) >>=
                 \case
                      (a, w) | w == mempty -> pure a
                             | otherwise   -> go (liftAssumptions a <> w)

assume :: MonadWriter AssumptionsAll m => Id -> Type -> m ()
assume v t = tell (AssumptionsAll (Map.singleton v [t]) mempty)

assume' :: MonadWriter AssumptionsAll m => Id -> StackDesc -> m ()
assume' v s = tell (AssumptionsAll mempty (Map.singleton v [s]))

_intersectionHandlesThisCase :: Type -> ()
_intersectionHandlesThisCase (PrimType {}) = ()
_intersectionHandlesThisCase (FunType {}) = ()
_intersectionHandlesThisCase (GroundVar {}) = ()
_intersectionHandlesThisCase (QuantVar {}) = ()

-- GroundVar and (non-TAny) PrimType are trivial as they're just an
-- equality check, which is handled at the very beginning.

unionStack :: (FromTypeError e, MonadError e m, MonadWriter AssumptionsAll m) =>
              StackDesc -> StackDesc -> m StackDesc
unionStack x y | x == y = pure x
unionStack (StackDesc (Stack []) (RestGround x)) (StackDesc (Stack []) (RestGround y))
    | x == y = pure $ StackDesc (Stack []) (RestGround x)
    | otherwise = throwError (fromTypeError $ CouldNotUnifyStack (StackDesc mempty (RestGround x))
                                                                 (StackDesc mempty (RestGround y)))
unionStack (StackDesc (Stack []) (RestQuant x)) y = assume' x y >> pure y
unionStack x (StackDesc (Stack []) (RestQuant y)) = assume' y x >> pure x
unionStack (StackDesc (Stack []) (RestGround x)) y =
    throwError (fromTypeError $ CouldNotUnifyStack (StackDesc mempty (RestGround x)) y)
unionStack x (StackDesc (Stack []) (RestGround y)) =
    throwError (fromTypeError $ CouldNotUnifyStack x (StackDesc mempty (RestGround y)))
unionStack (StackDesc (Stack (a:as)) x) (StackDesc (Stack (b:bs)) y) = do
  ab <- a `union` b
  StackDesc (Stack abs_) xy <- StackDesc (Stack as) x `unionStack` StackDesc (Stack bs) y
  return $ StackDesc (Stack (ab:abs_)) xy

union :: (FromTypeError e, MonadError e m, MonadWriter AssumptionsAll m) =>
         Type -> Type -> m Type
union a b | a == b = pure a
union (PrimType TNothing) t = pure t
union t (PrimType TNothing) = pure t
union (QuantVar a) (QuantVar b) =
    QuantVar a <$ (assume a (QuantVar b) >> assume b (QuantVar a))
union (QuantVar a) b = b <$ assume a b
union a (QuantVar b) = a <$ assume b a
union (FunType (FunctionType args1 rets1)) (FunType (FunctionType args2 rets2)) =
    liftA2 (fmap FunType . FunctionType) (intersectionStack args1 args2) (unionStack rets1 rets2)
union _ _ = pure (PrimType TAny)

intersectionStack :: (FromTypeError e, MonadError e m, MonadWriter AssumptionsAll m) =>
                     StackDesc -> StackDesc -> m StackDesc
intersectionStack x y | x == y = pure x
intersectionStack (StackDesc (Stack []) (RestGround x)) (StackDesc (Stack []) (RestGround y))
    | x == y = pure $ StackDesc (Stack []) (RestGround x)
    | otherwise = throwError (fromTypeError $ CouldNotUnifyStack (StackDesc mempty (RestGround x))
                                                                 (StackDesc mempty (RestGround y)))
intersectionStack (StackDesc (Stack []) (RestQuant x)) y = assume' x y >> pure y
intersectionStack x (StackDesc (Stack []) (RestQuant y)) = assume' y x >> pure x
intersectionStack (StackDesc (Stack []) (RestGround x)) y =
    throwError (fromTypeError $ CouldNotUnifyStack (StackDesc mempty (RestGround x)) y)
intersectionStack x (StackDesc (Stack []) (RestGround y)) =
    throwError (fromTypeError $ CouldNotUnifyStack x (StackDesc mempty (RestGround y)))
intersectionStack (StackDesc (Stack (a:as)) x) (StackDesc (Stack (b:bs)) y) = do
  ab <- a `intersection` b
  StackDesc (Stack abs_) xy <- StackDesc (Stack as) x `intersectionStack` StackDesc (Stack bs) y
  return $ StackDesc (Stack (ab:abs_)) xy

intersection :: (FromTypeError e, MonadError e m, MonadWriter AssumptionsAll m) =>
                Type -> Type -> m Type
intersection a b | a == b = pure a
intersection (PrimType TAny) t = pure t
intersection t (PrimType TAny) = pure t
intersection (QuantVar a) (QuantVar b) =
    QuantVar a <$ (assume a (QuantVar b) >> assume b (QuantVar a))
intersection (QuantVar a) b = b <$ assume a b
intersection a (QuantVar b) = a <$ assume b a
intersection (FunType (FunctionType args1 rets1)) (FunType (FunctionType args2 rets2)) =
    liftA2 (fmap FunType . FunctionType) (unionStack args1 args2) (intersectionStack rets1 rets2)
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
  cw <- consolidateUntilDone w
  let subAll :: Assumptions -> Type -> Type
      subAll (Assumptions m m') = substituteStackUntilDone m' . substituteUntilDone m
      u' = subAll cw u
      s' = subAll cw s
  if u' == s' then pure () else noUnify

-- pad :: FunctionType -> FunctionType -> (FunctionType, FunctionType)
-- pad (FunctionType a b) (FunctionType b' c) =
--     case Stack.length b `compare` Stack.length b' of
--       GT ->
--           let missing = Stack.length b - Stack.length b'
--               extra = fromJust $ Stack.takeBottom missing b
--           in (FunctionType a b, FunctionType (b' <> extra) (c <> extra))
--       LT ->
--           let missing = Stack.length b' - Stack.length b
--               extra = fromJust $ Stack.takeBottom missing b'
--           in (FunctionType (a <> extra) (b <> extra), FunctionType b' c)
--       EQ ->
--           -- They're already the correct dimensions, so pass through
--           (FunctionType a b, FunctionType b' c)

-- padPoly :: PolyFunctionType -> PolyFunctionType -> (PolyFunctionType, PolyFunctionType)
-- padPoly (PolyFunctionType x (FunctionType a b)) (PolyFunctionType y (FunctionType b' c)) =
--     case Stack.length b `compare` Stack.length b' of
--       GT ->
--           let missing = Stack.length b - Stack.length b'
--               extra = fromJust $ Stack.takeBottom missing b
--               y' = y `List.union` (x `List.intersect` (concatMap allQuantVars $ Stack.toList extra))
--           in (PolyFunctionType x (FunctionType a b), PolyFunctionType y' (FunctionType (b' <> extra) (c <> extra)))
--       LT ->
--           let missing = Stack.length b' - Stack.length b
--               extra = fromJust $ Stack.takeBottom missing b'
--               x' = x `List.union` (y `List.intersect` (concatMap allQuantVars $ Stack.toList extra))
--           in (PolyFunctionType x' (FunctionType (a <> extra) (b <> extra)), PolyFunctionType y (FunctionType b' c))
--       EQ ->
--           -- They're already the correct dimensions, so pass through
--           (PolyFunctionType x (FunctionType a b), PolyFunctionType y (FunctionType b' c))

requireSubtypeRest :: (FromTypeError e, MonadError e m, MonadWriter AssumptionsAll m) =>
                      RestVar -> RestVar -> m ()
requireSubtypeRest a b | a == b = pure ()
requireSubtypeRest (RestQuant a) (RestQuant b) =
    assume' a (StackDesc mempty $ RestQuant b) >> assume' b (StackDesc mempty $ RestQuant a)
requireSubtypeRest (RestQuant a) b = assume' a (StackDesc mempty b)
requireSubtypeRest a (RestQuant b) = assume' b (StackDesc mempty a)
requireSubtypeRest (RestGround a) (RestGround b)
    | a == b = pure ()
    | otherwise = throwError (fromTypeError $
                              CouldNotUnifyStack (StackDesc mempty $ RestGround a)
                                                 (StackDesc mempty $ RestGround b))

requireSubtypeStack :: (FromTypeError e, MonadError e m, MonadWriter AssumptionsAll m) =>
                       StackDesc -> StackDesc -> m ()
requireSubtypeStack (StackDesc xs x) (StackDesc ys y) = do
  let minlen = min (Stack.length xs) (Stack.length ys)
      (xs', xs1) = fromJust $ Stack.splitStack minlen xs
      (ys', ys1) = fromJust $ Stack.splitStack minlen ys
  zipWithM_ isSubtypeOf (Stack.toList xs') (Stack.toList ys')
  case (xs1, x, ys1, y) of
    (Stack [], x0, Stack [], y0) -> requireSubtypeRest x0 y0
    (xs1', RestGround x0, ys1', y0) ->
        throwError (fromTypeError $ CouldNotUnifyStack (StackDesc xs1' (RestGround x0))
                                                       (StackDesc ys1' y0))
    (xs1', x0, ys1', RestGround y0) ->
        throwError (fromTypeError $ CouldNotUnifyStack (StackDesc xs1' x0)
                                                       (StackDesc ys1' (RestGround y0)))
    (Stack [], RestQuant x0, ys1', y0) -> assume' x0 (StackDesc ys1' y0)
    (xs1', x0, Stack [], RestQuant y0) -> assume' y0 (StackDesc xs1' x0)
    (Stack (_:_), RestQuant _, Stack (_:_), RestQuant _) ->
        -- This case should never happen since we specifically split
        -- at the min length of the two stacks, so after splitting,
        -- the shorter of the two stacks must be empty.
        error "Assertion violated in requireSubtypeStack"

-- Left-to-right composition, so first function happens first.
composeFunctions :: (FromTypeError e, MonadError e m, MonadWriter AssumptionsAll m) =>
                    FunctionType -> FunctionType -> m FunctionType
composeFunctions (FunctionType a b) (FunctionType b' c) =
    requireSubtypeStack b b' >> pure (FunctionType a c)

composePFunctions :: (FromTypeError e, MonadError e m, MonadWriter AssumptionsAll m) =>
                     PolyFunctionType -> PolyFunctionType -> m PolyFunctionType
composePFunctions (PolyFunctionType i f) (PolyFunctionType j g) =
      go f (renameToAvoidConflicts' (`elem` i) g)
    where allQuants = i `List.union` (renameToAvoidConflicts'' (`elem` i) j)
          substituteBoth (Assumptions a b) = substituteUntilDone a . substituteStackUntilDone b
          go f0 g0 = do
            (h, AssumptionsAll w w') <- listen (composeFunctions f0 g0)
            let (univ , assum ) = Map.partitionWithKey (\k _ -> k `elem` allQuants) w
            let (univ', assum') = Map.partitionWithKey (\k _ -> k `elem` allQuants) w'
            -- The variables universally quantified by the functions
            -- should be substituted. Those that are still
            -- unrecognized should be passed through to the
            -- MonadWriter instance.
            tell $ AssumptionsAll assum assum'
            let asm = AssumptionsAll univ univ'
            casm <- consolidateUntilDone asm
            let h' = case substituteBoth casm (FunType h) of
                       FunType t -> t
                       _ -> error "Substitution changed type in composePFunctions"
                -- Intersect to remove any quantified variables that
                -- were instantiated by our assumptions
                ids = allQuants
            return $ PolyFunctionType ids h'
