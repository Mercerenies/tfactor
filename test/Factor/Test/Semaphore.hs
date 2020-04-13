
module Factor.Test.Semaphore where

import Factor.StdLib

import Data.IORef
import Control.Exception
import Control.Concurrent.QSem
import Control.Monad.Except

-- This is a helper file which is designed to ensure that we can share
-- the read-only portions of the VM (such as the Prelude and
-- Primitives modules) across test cases. It provides an IORef guarded
-- by a semaphore to control access.

data SharedPrelude = SharedPrelude (IORef (Maybe Prelude)) QSem

newSharedPrelude :: IO SharedPrelude
newSharedPrelude = SharedPrelude <$> newIORef Nothing <*> newQSem 1

getSharedPrelude :: SharedPrelude -> IO Prelude
getSharedPrelude (SharedPrelude ref sem) =
    bracket_ (waitQSem sem) (signalQSem sem) $ do
      value <- readIORef ref
      case value of
        Just x -> return x
        Nothing -> do
                   value' <- runExceptT loadPrelude >>= either (fail . show) pure
                   writeIORef ref (Just value')
                   return value'
