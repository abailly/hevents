module Hevents.Service  where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Hevents.Store


newtype ServiceT e s m a =
  ServiceT { runServiceT :: ExceptT e (ReaderT (TVar s) m) a }
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadError e)

instance MonadTrans (ServiceT e r) where
  lift = ServiceT . lift . lift

instance (Monad m) => MonadReader (TVar s) (ServiceT e s m) where
  f `local` ServiceT m = ServiceT $ f `local` m
  ask  = (ServiceT . lift) ask

instance (MonadIO m) => MonadIO (ServiceT e s m) where
  liftIO = lift . liftIO

instance (MonadStore m) => MonadStore (ServiceT e s m) where
  store = lift . store
  load  = liftIO . load

runService :: ServiceT e s m a -> TVar s -> m (Either e a)
runService effect = runReaderT (runExceptT . runServiceT $ effect)

gets :: (MonadIO m) => (s -> b) -> ServiceT e s m b
gets f = f <$> (ask >>= liftIO . readTVarIO)

modify :: (MonadIO m) => (s -> s) -> ServiceT e s m ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f
