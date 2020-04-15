{-# LANGUAGE FlexibleContexts #-}

module Factor.State.Resource(ResourceTable(..), RId, newResourceTable,
                             appendResource, getResource, getResource') where

import Factor.State.Types
import Factor.Error

import Data.Sequence(Seq(..), (!?))
import qualified Data.Sequence as Seq
import Control.Monad.Except

appendResource :: ReaderValue -> ResourceTable -> (RId, ResourceTable)
appendResource value (ResourceTable table) =
    let n = Seq.length table
    in (n, ResourceTable $ table :|> value)

getResource :: RId -> ResourceTable -> Maybe ReaderValue
getResource i (ResourceTable table) = table !? i

getResource' :: MonadError FactorError m => RId -> ResourceTable -> m ReaderValue
getResource' i t = case getResource i t of
                     Nothing -> throwError (InternalError $ "Invalid resource ID " ++ show i)
                     Just x -> pure x
