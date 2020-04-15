{-# LANGUAGE FlexibleContexts #-}

module Factor.State.Resource(ResourceTable(..), RId, newResourceTable,
                             appendResource, appendResource', appendResourceRO,
                             getResource, getResource',
                             defineResource, resourceCount, catResources,
                             modifyRIds) where

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

appendResource :: a -> ResourceTable a -> (RId, ResourceTable a)
appendResource value (ResourceTable table) =
    let n = Seq.length table
    in (n, ResourceTable $ table :|> value)

appendResource' :: MonadState (ResourceTable a) m => a -> m RId
appendResource' value = state (appendResource value)

appendResourceRO :: ReaderValue -> ReadOnlyState -> (RId, ReadOnlyState)
appendResourceRO value r =
    let (rid, table) = appendResource value $ r^.readerResources
    in (rid, set readerResources table r) -- TODO Can this be done strictly with lenses?

getResource :: RId -> ResourceTable a -> Maybe a
getResource i (ResourceTable table) = table !? i

getResource' :: MonadError FactorError m => RId -> ResourceTable a -> m a
getResource' i t = case getResource i t of
                     Nothing -> throwError (InternalError $ "Invalid resource ID " ++ show i)
                     Just x -> pure x

defineResource :: MonadState (ResourceTable a) m =>
                  Id -> a -> Map Id RId -> m (Map Id RId)
defineResource name r m = do
  id_ <- appendResource' r
  return $ Map.insert name id_ m

resourceCount :: ResourceTable a -> Int
resourceCount (ResourceTable table) = Seq.length table

catResources :: ResourceTable a -> ResourceTable a -> ResourceTable a
catResources (ResourceTable t) (ResourceTable t') = ResourceTable $ t >< t'

modifyRIdsMod :: (RId -> RId) -> Module -> Module
modifyRIdsMod f = moduleNames %~ fmap f

modifyRIds :: (RId -> RId) -> ResourceTable ReaderValue -> ResourceTable ReaderValue
modifyRIds f (ResourceTable table) = ResourceTable $ fmap go table
    where go (ModuleValue m) = ModuleValue $ modifyRIdsMod f m
          go (UDFunction t g) = UDFunction t g
          go (BIFunction t g) = BIFunction t g
          go (UDMacro t m) = UDMacro t m
