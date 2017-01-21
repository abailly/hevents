{-# LANGUAGE DeriveGeneric #-}

{-| Interface and types for persisting events in an underlying `Store` -}
module Hevents.Eff.Store(module Hevents.Eff.Store.Events,
                          Reader,StoreError(..),Count(..),Offset(..),
                          Versionable(..), Version(..),
                          StoreOperation(..), StorageResult(..), Store(..)
                          ) where

import           Data.ByteString          (ByteString)
import           Data.Int
import           Data.Serialize
import           Data.Text                (Text)
import           Data.Text.Encoding
import           GHC.Generics
import           Hevents.Eff.Store.Events

class Store m store  where
  close :: store -> m store
  store :: (Versionable event) =>
           store  -- ^Storage Engine
        -> m (Either error event)
        -- ^Pre-treatment action that returns something to serialize or an error that is passed down to post
        -- as is
        -> (Either error (StorageResult event) -> m result)
        -- ^Post-treatment action that provides some result out of storage result or error in pre-treatment
        -> m (StorageResult result)
  load  :: (Versionable event) => store -> m (StorageResult event)
  reset :: store -> m (StorageResult ())


newtype Offset = Offset { offset :: Int64 } deriving (Eq, Ord, Show, Read, Serialize, Num)
newtype Count  = Count { count :: Int64 } deriving (Eq, Ord, Show, Read, Serialize, Num)

data StoreError = IOError { reason :: !Text } deriving (Show, Generic)

instance Serialize StoreError where
  put (IOError t) = put (encodeUtf8 t)
  get             = IOError . decodeUtf8 <$> get


type Reader a = ByteString -> Either String a

-- |Operations provided by the store
data StoreOperation m s where
  OpStore  :: Versionable s
    => { pre :: m (Either a s) -- ^ Pre-treatment action that returns something to serialize or an error
       , post :: (Either a (StorageResult s) -> m r)
       } -> StoreOperation m r
  OpLoad   :: Versionable s => StoreOperation m s
  OpReset  :: StoreOperation m s

-- |Result of storage operations.
data StorageResult s where
  OpFailed     :: { failureReason :: String } -> StorageResult s
  WriteSucceed :: s -> StorageResult s
  WriteFailed  :: s -> StorageResult s
  LoadSucceed  :: (Versionable s) => [s] -> StorageResult s
  ResetSucceed :: StorageResult s
  NoOp         :: StorageResult s

instance (Show s) => Show (StorageResult s) where
  show (OpFailed r)     = "OpFailed " ++  r
  show (WriteSucceed s) = "WriteSucceed " ++ show s
  show (WriteFailed f)  = "WriteSucceed " ++ show f
  show (LoadSucceed ss) = "LoadSucceed " ++ show (length ss)
  show ResetSucceed     = "ResetSucceed"
  show NoOp             = "NoOp"

