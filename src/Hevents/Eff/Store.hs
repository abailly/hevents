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

class Store m s  where
  close :: s -> m s
  store :: (Versionable e) => e -> s -> m (StorageResult e)
  load  :: (Versionable e) => s -> m (StorageResult e)
  reset :: s -> m (StorageResult ())
  writeCustom :: (Versionable e)
    => m (Either a e)
    -- ^Pre-treatment action that returns something to serialize or an error that is passed down to post
    -- as is
    -> (Either a (StorageResult e) -> m r)
    -- ^Post-treatment action that provides some result out of storage result or error in pre-treatment
    -> s
    -- ^Storage Engine
    -> m r

newtype Offset = Offset { offset :: Int64 } deriving (Eq, Ord, Show, Read, Serialize, Num)
newtype Count  = Count { count :: Int64 } deriving (Eq, Ord, Show, Read, Serialize, Num)

data StoreError = IOError { reason :: !Text } deriving (Show, Generic)

instance Serialize StoreError where
  put (IOError t) = put (encodeUtf8 t)
  get             = IOError . decodeUtf8 <$> get

class (Serialize s) => Versionable s where
  write :: Version -> s -> ByteString
  write _ = runPut . put
  read :: Version -> ByteString -> Either String s
  read _ = runGet get

instance Versionable () where
  write = undefined
  read = undefined

newtype Version  = Version { version :: Int } deriving (Eq,Show, Num)

type Reader a = ByteString -> Either String a

type StoreResult a = Either StoreError a

-- |Operations provided by the store
data StoreOperation s where
  OpStore  :: Versionable s => s -> StoreOperation s
  OpLoad   :: Versionable s => StoreOperation s
  OpReset  :: StoreOperation s

-- |Result of storage operations.
data StorageResult s where
  OpFailed     :: { failureReason :: String } -> StorageResult s
  WriteSucceed :: (Versionable s) => s -> Int -> StorageResult s
  LoadSucceed  :: (Versionable s) => [s] -> StorageResult s
  ResetSucceed :: StorageResult s
  NoOp         :: StorageResult s
