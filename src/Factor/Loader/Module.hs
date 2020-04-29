{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Factor.Loader.Module where

import Factor.State.Reader
import Factor.State.Resource
import Factor.Id
import Factor.Util
import Factor.Util.Graph(Graph, Cycle(..))
import qualified Factor.Util.Graph as Graph
import Factor.Error
import Factor.Trait.Types
import Factor.Trait.Functor

import Control.Monad.Except
import Control.Monad.State
import Control.Lens hiding (children)
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Either(isRight)

-- Right now, we need no additional information.
newtype GraphEdge = GraphEdge QId
    deriving (Show, Eq, Ord)

resolveModuleDecl :: (MonadError FactorError m, MonadState ReadOnlyState m) =>
                     QId -> ModuleDecl -> Module -> m Module
resolveModuleDecl _mqid decl m =
    case decl of
      IncludeModule dest -> view' (possibly $ atQId dest) >>= \case
                            Just (ModuleValue m') ->
                                let go m1 (k, v) = forOf (moduleNames.at k) m1 $ \case
                                                   Nothing -> pure (Just v)
                                                   Just _ -> throwError (DuplicateDecl k)
                                in foldM go m (Map.toList $ m'^.moduleNames)
                            _ -> throwError (NoSuchModule dest)
      Alias _ _ -> pure m
      Open _ -> pure m
      AssertTrait _ -> pure m
    where view' x = view x <$> get

loadModule :: (MonadError FactorError m, MonadState ReadOnlyState m) => QId -> Module -> m Module
loadModule mqid m = foldM (flip $ resolveModuleDecl mqid) m (m^.moduleDecls)

loadModuleAt :: MonadError FactorError m => QId -> ReadOnlyState -> m ReadOnlyState
loadModuleAt qid r =
  case r^.possibly (atQId qid) of
    Nothing -> throwError (NoSuchModule qid)
    Just (UDFunction {}) -> pure r
    Just (BIFunction {}) -> pure r
    Just (UDMacro {}) -> pure r
    Just (ModuleValue m) -> runStateT (loadModule qid m) r >>= \(m', r') ->
                            return (set (atQId qid) (ModuleValue m') r')
    Just (TraitValue {}) -> pure r
    Just (FunctorValue {}) -> pure r -- TODO Nothing to do here right now, but there will be later.
    Just (TypeValue {}) -> pure r
    Just (SynonymPlaceholder (SynonymGeneral target)) ->
        case view (possibly $ atQIdResource target) r of
          Nothing -> throwError (NoSuchModule target)
          Just rid -> return (set (atQIdResource qid) rid r)
    Just (SynonymPlaceholder (ActualizeFunctor (TraitRef dest args))) ->
        lookupFn dest r >>= \case
                 FunctorValue pm -> do
                      (rid, r') <- runStateT (bindModule qid dest pm args) r
                      return (set (atQIdResource qid) rid r')
                 _ -> throwError (NoSuchModule dest)

dependenciesFromModuleDecl :: ModuleDecl -> [GraphEdge]
dependenciesFromModuleDecl (IncludeModule dest) = [GraphEdge dest' | dest' <- allPrefixes dest]
dependenciesFromModuleDecl (Alias _ _) = []
dependenciesFromModuleDecl (Open _) = []
dependenciesFromModuleDecl (AssertTrait _) = []

-- When we're referencing something deep inside another module, the
-- resource in question may not technically exist yet. In this case,
-- we should have a dependency based on the "first hit" rule, i.e. we
-- should depend on the innermost name which currently exists. For
-- example, if I reference the type named A.B.C.t, and no such
-- resource exists, I should check A.B.C to see if it exists (for it
-- may be a functor actualization or a synonym). Then I should check
-- A.B, then A, and take the first of those which does exist as a
-- dependency.

firstHit :: ReadOnlyState -> QId -> Maybe QId
firstHit r qid = List.find exists $ reverse (tail (allPrefixes qid))
    where exists q = isRight $ lookupFn q r

firstHitEdge :: ReadOnlyState -> QId -> QId -> [(QId, GraphEdge)]
firstHitEdge r q0 q1 = case firstHit r q1 of
                         Nothing -> []
                         Just q1' -> [(q0, GraphEdge q1')]

produceModuleDepGraph :: [QId] -> ReadOnlyState -> Graph QId GraphEdge
produceModuleDepGraph qids reader =
    Graph.fromEdges qids (concat . mapWithQId go $ reader^.readerResources) proj
    where proj (GraphEdge q) = q
          go (qid, r) =
              case r of
                UDFunction {} -> []
                BIFunction {} -> []
                UDMacro {} -> []
                ModuleValue m ->
                    let fromdecls = [(qid, e) | decl <- m^.moduleDecls
                                              , e <- dependenciesFromModuleDecl decl]
                        children = [(qid, GraphEdge e) | (k, _) <- Map.toList (m^.moduleNames)
                                                       , let e = qid <> QId [k]]
                    in fromdecls ++ children
                TraitValue {} -> []
                FunctorValue {} -> [] -- TODO This
                TypeValue {} -> []
                SynonymPlaceholder t ->
                  case t of
                    SynonymGeneral q -> firstHitEdge reader qid q
                    ActualizeFunctor (TraitRef q ts) ->
                        concatMap (firstHitEdge reader qid) (q : ts)

-- As in Factor.Loader.Graph, we reverse the top sort order since we
-- want to load dependencies BEFORE the things that depend upon them.
determineValidLoadOrder :: MonadError FactorError m => Graph QId GraphEdge -> m [QId]
determineValidLoadOrder g
    | (Cycle vs _ : _) <- Graph.findCycles g = throwError (LoadCycle vs)
    | otherwise = case Graph.topSort g of
                    Nothing -> throwError $ InternalError "Error in determineValidLoadOrder"
                    Just order -> pure $ reverse order

loadModules :: MonadError FactorError m => [QId] -> ReadOnlyState -> m ReadOnlyState
loadModules qids r = do
  let graph = produceModuleDepGraph qids r
  loadOrder <- determineValidLoadOrder graph
  foldM (flip loadModuleAt) r loadOrder
