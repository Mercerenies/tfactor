{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleInstances,
  MultiParamTypeClasses, DeriveFunctor, UndecidableInstances, DefaultSignatures, GADTs #-}

module Factor.State.Alias.New where

import Factor.Id

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Morph
import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.Cont
import Control.Monad.Trans.Maybe
import Data.Map(Map)

-- Eventually, this file will just be Factor.State.Alias. For
-- compatibility reasons, we're keeping that file around for now. But
-- eventually, this will subsume and replace it.

-- I'm making a custom monad here. It's the state monad with an alias
-- table state, but I want it to be able to coexist with other state
-- monads, so it explicitly doesn't respond to MonadState. You can
-- ONLY use the functions in this module to access it.

type Alias = QId

newtype AliasesT m a = AliasesT { runAliasesT :: StateT (Map Id Alias) m a }
    deriving (Functor, Applicative, Monad, MonadTrans, MFunctor, MonadFix,
              MonadReader r, MonadWriter w, MonadError e, MonadRWS r w s,
              MonadFail, MonadIO, MonadCont, Alternative, MonadPlus)

instance MonadState s m => MonadState s (AliasesT m) where
    state s = lift (state s)
    get = lift get
    put s = lift (put s)

class Monad m => MonadAliases m where
    getAliases :: m (Map Id Alias)
    putAliases :: Map Id Alias -> m ()
    stateAliases :: (Map Id Alias -> (a, Map Id Alias)) -> m a

    default getAliases :: (MonadTrans t, MonadAliases m', m ~ t m') => m (Map Id Alias)
    getAliases = lift getAliases
    default putAliases :: (MonadTrans t, MonadAliases m', m ~ t m') => Map Id Alias -> m ()
    putAliases m = lift (putAliases m)
    default stateAliases :: (MonadTrans t, MonadAliases m', m ~ t m') =>
                            (Map Id Alias -> (a, Map Id Alias)) -> m a
    stateAliases s = lift (stateAliases s)

instance Monad m => MonadAliases (AliasesT m) where
    getAliases = AliasesT get
    putAliases m = AliasesT (put m)
    stateAliases s = AliasesT (state s)

instance MonadAliases m => MonadAliases (ReaderT r m)
instance (Monoid w, MonadAliases m) => MonadAliases (WriterT w m)
instance MonadAliases m => MonadAliases (StateT s m)
instance (Monoid w, MonadAliases m) => MonadAliases (RWST r w s m)
instance MonadAliases m => MonadAliases (ExceptT e m)
instance MonadAliases m => MonadAliases (ContT r m)
instance MonadAliases m => MonadAliases (MaybeT m)
