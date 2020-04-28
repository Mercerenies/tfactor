{-# LANGUAGE FlexibleContexts, LambdaCase, ScopedTypeVariables #-}

module Factor.Type.Unify where

import Factor.Util
import Factor.Id
import Factor.Type
import Factor.Type.Error
import Factor.Error
import Factor.State.Reader
import Factor.Stack(Stack(..))
import qualified Factor.Stack as Stack
import qualified Factor.Util.Graph as Graph

import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.Reader hiding (reader)
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

findFirstCycle :: Eq a => [a] -> Maybe [a]
findFirstCycle = go []
    where go _ [] = Nothing
          go ys (x:xs)
              | Just i <- x `List.elemIndex` ys = Just (take (i + 1) ys)
              | otherwise = go (x:ys) xs

leadsBackMin :: Map Id Type -> Id -> Bool
leadsBackMin m i0 = case findFirstCycle $ iterate go (Just i0) of
                   Just cycl | maximum cycl == Just i0 -> True
                   _ -> False
    where go Nothing = Nothing
          go (Just i)
             | Just (QuantVar i') <- Map.lookup i m = Just i'
             | otherwise = Nothing

leadsBackMin' :: Map Id StackDesc -> Id -> Bool
leadsBackMin' m i0 = case findFirstCycle $ iterate go (Just i0) of
                   Just cycl | maximum cycl == Just i0 -> True
                   _ -> False
    where go Nothing = Nothing
          go (Just i)
             | Just (StackDesc (Stack []) (RestQuant i')) <- Map.lookup i m = Just i'
             | otherwise = Nothing

-- This isn't a full cycle check. It deals with trivial solvable
-- cases. We need to do a proper occurs check for the cycles that
-- should err.
removeSynonyms :: Assumptions -> Assumptions
removeSynonyms (Assumptions m0 m1) = Assumptions (removeSynonyms0 m0) (removeSynonyms1 m1)
    where removeSynonyms0 m =
              let classify k (QuantVar _) | leadsBackMin m k = True
                  classify _ _ = False
                  (_removals, keeps) = Map.partitionWithKey classify m
--                  keeps' = fmap (substituteUntilDone removals) keeps
              in keeps
          removeSynonyms1 :: Map Id StackDesc -> Map Id StackDesc
          removeSynonyms1 m =
              let classify k (StackDesc (Stack []) (RestQuant _))
                      | leadsBackMin' m k = True
                  classify _ _ = False
                  (_removals, keeps) = Map.partitionWithKey classify m
--                  keeps' = fmap (substituteStackUntilDone' removals) keeps
              in keeps

substituteFully :: Assumptions -> Type -> Type
substituteFully asm t0 =
    let Assumptions m m' = removeSynonyms asm
        go t = let t' = substituteUntilDone m . substituteStackUntilDone m' $ t
               in if t == t' then t else go t'
    in go t0

substituteFully' :: Assumptions -> StackDesc -> StackDesc
substituteFully' asm t0 =
    let Assumptions m m' = removeSynonyms asm
        go t = let t' = substituteUntilDone' m . substituteStackUntilDone' m' $ t
               in if t == t' then t else go t'
    in go t0

occursCheck :: (FromTypeError e, MonadError e m) => Assumptions -> m ()
occursCheck (Assumptions m m') =
    let allQuantVars' (StackDesc (Stack ts) r) =
            concatMap allQuantVars ts ++ (case r of { RestQuant r' -> [r'] ; _ -> [] })
        compile f g n = [(v, e) | (v, t) <- Map.assocs n
                                , e <- fmap ((,) (g t)) $ f t]
        vertices = Map.keys m ++ Map.keys m'
        edges = compile allQuantVars Right m ++ compile allQuantVars' Left m'
        graph = Graph.fromEdges vertices edges snd
    in case Graph.findCycles graph of
         [] -> pure ()
         ( Graph.Cycle vs es : _ ) ->
             let zipped = zip vs (fmap fst es) in
             throwError (fromTypeError $ OccursCheck zipped)

consolidate :: (MonadError FactorError m,
                MonadWriter AssumptionsAll m, MonadReader ReadOnlyState m) =>
               AssumptionsAll -> m Assumptions
consolidate (AssumptionsAll m0 m1) =
    Assumptions <$>
        mapM (foldM1 unify) m0 <*>
        mapM (foldM1 unifyStack) m1

consolidateUntilDone :: (MonadError FactorError m, MonadReader ReadOnlyState m) =>
                        AssumptionsAll -> m Assumptions
consolidateUntilDone asm = fmap removeSynonyms (go asm) >>= \asm' -> occursCheck asm' >> pure asm'
    where go x = runWriterT (consolidate x) >>=
                 \case
                      (a, w) | w == mempty -> pure a
                             | otherwise   ->
                                 let a' = removeSynonyms a
                                     AssumptionsAll w0 w1 = w
                                     sub  = fmap (fmap (substituteFully  a'))
                                     sub' = fmap (fmap (substituteFully' a'))
                                     w' = AssumptionsAll (sub w0) (sub' w1)
                                 in go (liftAssumptions a <> w')

assume :: MonadWriter AssumptionsAll m => Id -> Type -> m ()
assume v t = tell (AssumptionsAll (Map.singleton v [t]) mempty)

assume' :: MonadWriter AssumptionsAll m => Id -> StackDesc -> m ()
assume' v s = tell (AssumptionsAll mempty (Map.singleton v [s]))

_unifyHandlesThisCase :: Type -> ()
_unifyHandlesThisCase (FunType {}) = ()
_unifyHandlesThisCase (NamedType {}) = ()
_unifyHandlesThisCase (GroundVar {}) = ()
_unifyHandlesThisCase (QuantVar {}) = ()

-- GroundVar is trivial as it's just an equality check, which is
-- handled at the very beginning.

canon :: (MonadReader ReadOnlyState m, MonadError FactorError m) => QId -> m QId
canon qid = ask >>= lookupFnName qid

unifyStack :: (MonadError FactorError m,
               MonadWriter AssumptionsAll m, MonadReader ReadOnlyState m) =>
              StackDesc -> StackDesc -> m StackDesc
unifyStack x y | x == y = pure x
unifyStack (StackDesc (Stack []) (RestGround x)) (StackDesc (Stack []) (RestGround y))
    | x == y = pure $ StackDesc (Stack []) (RestGround x)
    | otherwise = throwError (fromTypeError $ CouldNotUnifyStack (StackDesc mempty (RestGround x))
                                                                 (StackDesc mempty (RestGround y)))
unifyStack (StackDesc (Stack []) (RestQuant x)) y = assume' x y >> pure y
unifyStack x (StackDesc (Stack []) (RestQuant y)) = assume' y x >> pure x
unifyStack (StackDesc (Stack []) (RestGround x)) y =
    throwError (fromTypeError $ CouldNotUnifyStack (StackDesc mempty (RestGround x)) y)
unifyStack x (StackDesc (Stack []) (RestGround y)) =
    throwError (fromTypeError $ CouldNotUnifyStack x (StackDesc mempty (RestGround y)))
unifyStack (StackDesc (Stack (a:as)) x) (StackDesc (Stack (b:bs)) y) = do
  ab <- a `unify` b
  StackDesc (Stack abs_) xy <- StackDesc (Stack as) x `unifyStack` StackDesc (Stack bs) y
  return $ StackDesc (Stack (ab:abs_)) xy

unify :: (MonadError FactorError m,
          MonadWriter AssumptionsAll m, MonadReader ReadOnlyState m) =>
         Type -> Type -> m Type
unify a b | a == b = pure a
unify (QuantVar a) (QuantVar b) =
    QuantVar a <$ (assume a (QuantVar b) >> assume b (QuantVar a))
unify (QuantVar a) b = b <$ assume a b
unify a (QuantVar b) = a <$ assume b a
unify (NamedType (TypeId a as)) (NamedType (TypeId b bs)) = do
  a' <- canon a
  b' <- canon b
  if a' == b' && length as == length bs then
      NamedType . TypeId a' <$> zipWithM unify as bs
  else
      throwError (fromTypeError $ CouldNotUnify (NamedType (TypeId a as)) (NamedType (TypeId b bs)))
unify (FunType (FunctionType args1 rets1)) (FunType (FunctionType args2 rets2)) =
    liftA2 (fmap FunType . FunctionType) (unifyStack args1 args2) (unifyStack rets1 rets2)
unify a b = throwError (fromTypeError $ CouldNotUnify a b)

canUnifyFn :: (MonadError FactorError m,
               MonadWriter AssumptionsAll m, MonadReader ReadOnlyState m) =>
               FunctionType -> FunctionType -> m ()
canUnifyFn = canUnify `on` FunType

canUnify :: (MonadError FactorError m,
             MonadWriter AssumptionsAll m, MonadReader ReadOnlyState m) =>
             Type -> Type -> m ()
canUnify s t = do
  (_, w) <- listen (s `unify` t)
  -- Consolidation also performs occurs check, which we still want to do.
  Assumptions {} <- consolidateUntilDone w
  pure ()

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
--               y' = y `List.unify` (x `List.intersect` (concatMap allQuantVars $ Stack.toList extra))
--           in (PolyFunctionType x (FunctionType a b), PolyFunctionType y' (FunctionType (b' <> extra) (c <> extra)))
--       LT ->
--           let missing = Stack.length b' - Stack.length b
--               extra = fromJust $ Stack.takeBottom missing b'
--               x' = x `List.unify` (y `List.intersect` (concatMap allQuantVars $ Stack.toList extra))
--           in (PolyFunctionType x' (FunctionType (a <> extra) (b <> extra)), PolyFunctionType y (FunctionType b' c))
--       EQ ->
--           -- They're already the correct dimensions, so pass through
--           (PolyFunctionType x (FunctionType a b), PolyFunctionType y (FunctionType b' c))

requireSubtypeRest :: (FromTypeError e, MonadError e m,
                       MonadWriter AssumptionsAll m, MonadReader ReadOnlyState m) =>
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

requireSubtypeStack :: (MonadError FactorError m,
                        MonadWriter AssumptionsAll m, MonadReader ReadOnlyState m) =>
                       StackDesc -> StackDesc -> m ()
requireSubtypeStack (StackDesc xs x) (StackDesc ys y) = do
  let minlen = min (Stack.length xs) (Stack.length ys)
      (xs', xs1) = fromJust $ Stack.splitStack minlen xs
      (ys', ys1) = fromJust $ Stack.splitStack minlen ys
  zipWithM_ canUnify (Stack.toList xs') (Stack.toList ys')
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
composeFunctions :: (MonadError FactorError m,
                     MonadWriter AssumptionsAll m, MonadReader ReadOnlyState m) =>
                    FunctionType -> FunctionType -> m FunctionType
composeFunctions (FunctionType a b) (FunctionType b' c) =
    requireSubtypeStack b b' >> pure (FunctionType a c)

composePFunctions :: (MonadError FactorError m,
                      MonadWriter AssumptionsAll m, MonadReader ReadOnlyState m) =>
                     PolyFunctionType -> PolyFunctionType -> m PolyFunctionType
composePFunctions (PolyFunctionType i f) (PolyFunctionType j g) =
      go f (renameToAvoidConflicts' (`elem` i) g)
    where allQuants = i `List.union` (renameToAvoidConflicts'' (`elem` i) j)
          go f0 g0 = do
            (h, AssumptionsAll w w') <- censor (const mempty) $ listen (composeFunctions f0 g0)
            let (univ , assum ) = Map.partitionWithKey (\k _ -> k `elem` allQuants) w
            let (univ', assum') = Map.partitionWithKey (\k _ -> k `elem` allQuants) w'
            -- The variables universally quantified by the functions
            -- should be substituted. Those that are still
            -- unrecognized should be passed through to the
            -- MonadWriter instance.
            tell $ AssumptionsAll assum assum'
            let asm = AssumptionsAll univ univ'
            casm <- consolidateUntilDone asm
            let h' = case substituteFully casm (FunType h) of
                       FunType t -> t
                       _ -> error "Substitution changed type in composePFunctions"
                -- Intersect to remove any quantified variables that
                -- were instantiated by our assumptions
                ids = allQuants
            return $ PolyFunctionType ids h'
