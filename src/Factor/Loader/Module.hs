{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Factor.Loader.Module where

import Factor.State
import Factor.State.Resource
import Factor.Id
import Factor.Util
import Factor.Util.Graph(Graph, Cycle(..))
import qualified Factor.Util.Graph as Graph
import Factor.Error

import Control.Monad.Except
import Control.Monad.Reader hiding (reader)
import Control.Lens

-- Right now, we need no additional information.
newtype GraphEdge = GraphEdge QId
    deriving (Show, Eq, Ord)

loadModule :: (MonadError FactorError m, MonadReader ReadOnlyState m) => RId -> m RId
loadModule r = view (readerResources . possibly (ix r)) >>= \case
               Nothing -> throwError (InternalError $ "No such resource " ++ show r)
               Just (UDFunction {}) -> pure r
               Just (BIFunction {}) -> pure r
               Just (UDMacro {}) -> pure r
               Just (ModuleValue {}) -> pure r
               Just (ModuleSynonym dest) ->
                   view (possibly $ atQIdResource dest) >>= \case
                        Nothing -> throwError (NoSuchModule dest)
                        Just r' -> pure r'

loadModuleAt :: MonadError FactorError m => QId -> ReadOnlyState -> m ReadOnlyState
loadModuleAt qid r = traverseOf (atQIdResource qid) (\v -> runReaderT (loadModule v) r) r

produceModuleDepGraph :: [QId] -> ReadOnlyState -> Graph QId GraphEdge
produceModuleDepGraph qids reader =
    Graph.fromEdges qids (concat . mapWithQId go $ reader^.readerResources) proj
    where proj (GraphEdge q) = q
          go (qid, r) =
              let parents = [(qid, GraphEdge qid') | qid' <- nonemptyPrefixes qid]
                  others = case r of
                             UDFunction {} -> []
                             BIFunction {} -> []
                             UDMacro {} -> []
                             ModuleValue {} -> []
                             ModuleSynonym dest -> [(qid, GraphEdge dest)]
              in parents ++ others

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
