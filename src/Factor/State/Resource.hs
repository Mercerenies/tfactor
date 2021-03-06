{-# LANGUAGE FlexibleContexts #-}

module Factor.State.Resource(ResourceTable(..), RId, newResourceTable,
                             appendResource, appendResource', appendResourceRO,
                             getResourceFull, getResource, getResourceName,
                             getResource', getResourceName',
                             modifyResourceFull, modifyResource,
                             defineResource, resourceCount, catResources,
                             modifyRIds, traverseWithQId, mapWithQId) where

-- TODO Should we give the top-level module a place in the resource
-- table? It may make merging modules more of a pain, since the merge
-- operation at the top-level is nontrivial whereas for all other
-- modules it's a simple error.

import Factor.State.Types
import Factor.Error
import Factor.Id

import Data.Sequence(Seq(..), (!?), (><))
import qualified Data.Sequence as Seq
import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.State
import Control.Lens

appendResource :: QId -> a -> ResourceTable a -> (RId, ResourceTable a)
appendResource qid value (ResourceTable table) =
    let n = Seq.length table
    in (n, ResourceTable $ table :|> (qid, value))

appendResource' :: MonadState (ResourceTable a) m => QId -> a -> m RId
appendResource' qid value = state (appendResource qid value)

appendResourceRO :: QId -> ReaderValue -> ReadOnlyState -> (RId, ReadOnlyState)
appendResourceRO qid value r =
    let (rid, table) = appendResource qid value $ r^.readerResources
    in (rid, set readerResources table r) -- TODO Can this be done strictly with lenses?

getResourceFull :: RId -> ResourceTable a -> Maybe (QId, a)
getResourceFull i (ResourceTable table) = table !? i

getResource :: RId -> ResourceTable a -> Maybe a
getResource i (ResourceTable table) = fmap snd $ table !? i

getResourceName :: RId -> ResourceTable a -> Maybe QId
getResourceName i (ResourceTable table) = fmap fst $ table !? i

getResource' :: MonadError FactorError m => RId -> ResourceTable a -> m a
getResource' i t = case getResource i t of
                     Nothing -> throwError (InternalError $ "Invalid resource ID " ++ show i)
                     Just x -> pure x

getResourceName' :: MonadError FactorError m => RId -> ResourceTable a -> m QId
getResourceName' i t = case getResourceName i t of
                         Nothing -> throwError (InternalError $ "Invalid resource ID " ++ show i)
                         Just x -> pure x

modifyResource :: (a -> a) -> RId -> ResourceTable a -> ResourceTable a
modifyResource f i (ResourceTable table) = ResourceTable $ Seq.adjust (_2 %~ f) i table

modifyResourceFull :: ((QId, a) -> (QId, a)) -> RId -> ResourceTable a -> ResourceTable a
modifyResourceFull f i (ResourceTable table) = ResourceTable $ Seq.adjust f i table

defineResource :: MonadState (ResourceTable a) m =>
                  QId -> Id -> a -> Map Id RId -> m (Map Id RId)
defineResource qid name r m = do
  id_ <- appendResource' qid r
  return $ Map.insert name id_ m

resourceCount :: ResourceTable a -> Int
resourceCount (ResourceTable table) = Seq.length table

catResources :: ResourceTable a -> ResourceTable a -> ResourceTable a
catResources (ResourceTable t) (ResourceTable t') = ResourceTable $ t >< t'

modifyRIdsMod :: (RId -> RId) -> Module -> Module
modifyRIdsMod f = moduleNames %~ fmap f

modifyRIds :: (RId -> RId) -> ResourceTable ReaderValue -> ResourceTable ReaderValue
modifyRIds f (ResourceTable table) = ResourceTable $ fmap (over _2 go) table
    where go (ModuleValue m) = ModuleValue $ modifyRIdsMod f m
          go (UDFunction t g) = UDFunction t g
          go (BIFunction t g) = BIFunction t g
          go (UDMacro t m) = UDMacro t m
          go (TraitValue t) = TraitValue t
          go (FunctorValue m) = FunctorValue m -- Nothing in here has an allocated RId yet,
                                               -- so there's nothing to modify.
          go (TypeValue ti) = TypeValue ti
          go (SynonymPlaceholder t) = SynonymPlaceholder t -- Like with functors, nothing
                                                           -- to modify yet.

traverseWithQId :: Applicative f => ((QId, a) -> f b) -> ResourceTable a -> f (ResourceTable b)
traverseWithQId f (ResourceTable table) =
    ResourceTable <$> traverse (\(q, a) -> fmap ((,) q) $ f (q, a)) table

mapWithQId :: ((QId, a) -> b) -> ResourceTable a -> ResourceTable b
mapWithQId f = runIdentity . traverseWithQId (Identity . f)
