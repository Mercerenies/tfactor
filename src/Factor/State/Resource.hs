{-# LANGUAGE FlexibleContexts #-}

module Factor.State.Resource(ResourceTable(..), RId, newResourceTable,
                             appendResource, appendResource', appendResourceRO,
                             getResource, getResourceName, getResource', getResourceName',
                             defineResource, resourceCount, catResources,
                             modifyRIds, traverseWithQId, mapWithQId) where

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
          go (ModuleSynonym q) = ModuleSynonym q

traverseWithQId :: Applicative f => ((QId, a) -> f b) -> ResourceTable a -> f (ResourceTable b)
traverseWithQId f (ResourceTable table) =
    ResourceTable <$> traverse (\(q, a) -> fmap ((,) q) $ f (q, a)) table

mapWithQId :: ((QId, a) -> b) -> ResourceTable a -> ResourceTable b
mapWithQId f = runIdentity . traverseWithQId (Identity . f)
