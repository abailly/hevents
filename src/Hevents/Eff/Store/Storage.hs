module Hevents.Eff.Store.Storage where

newtype MemoryStorage = MemoryStorage { mem :: TVar [ ByteString ] }

instance Storage MemoryStorage where
  persist MemoryStorage{..} (Store x k)    = lift (modifyTVar' mem (runPut (put x):) >> return Nothing) >>= k
  persist MemoryStorage{..} (Load o c g k) = lift (checkErrors . map g <$> readTVar mem) >>= k
    where
      checkErrors xs = case partitionEithers xs of
        ([],rs)   -> Right $ reverse $ rs
        ((e:_),_) -> Left  $ IOError $ pack e
  persist MemoryStorage{..} (Reset k)      = lift (writeTVar mem [] >> return Nothing) >>= k

