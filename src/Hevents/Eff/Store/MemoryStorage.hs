module Hevents.Eff.Store.MemoryStorage where

import           Control.Concurrent.STM
import           Control.Eff.Lift
import           Data.ByteString        (ByteString)
import           Data.Either
import           Data.Serialize
import           Data.Text              (pack)
import           Hevents.Eff.Store

newtype MemoryStorage = MemoryStorage { mem :: TVar [ ByteString ] }

instance Storage STM MemoryStorage where
  persist MemoryStorage{..} (Store x k)    = lift (modifyTVar' mem (runPut (put x):) >> return (Right x)) >>= k
  persist MemoryStorage{..} (Load Offset{..} Count{..} g k) = lift (checkErrors . map g <$> readTVar mem) >>= k
    where
      checkErrors xs = case partitionEithers xs of
        ([],rs)   -> Right $ reverse $ take (fromIntegral count) $ drop (fromIntegral offset) $ rs
        ((e:_),_) -> Left  $ IOError $ pack e
  persist MemoryStorage{..} (Reset k)      = lift (writeTVar mem [] >> return (Right ())) >>= k


makeMemoryStore :: STM MemoryStorage
makeMemoryStore = MemoryStorage <$> newTVar []
