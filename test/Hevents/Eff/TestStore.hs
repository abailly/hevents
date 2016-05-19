module Hevents.Eff.TestStore(SomeString(..),
                             makeMemoryStore,FallibleStorage,readMemoryStore) where

import           Control.Concurrent.STM
import           Control.Eff.Lift
import qualified Data.ByteString        as BS
import           Data.Either
import           Data.Serialize
import qualified Data.Text              as T
import           Data.Text.Encoding
import           Hevents.Eff            hiding (makeMemoryStore, mem)
import           System.Clock
import           Test.QuickCheck

instance Arbitrary SHA1 where
  arbitrary = SHA1 . BS.pack <$> vectorOf 20 arbitrary

instance Arbitrary TimeSpec where
  arbitrary = TimeSpec <$> arbitrary <*> arbitrary

instance Arbitrary EventVersion where
  arbitrary = EventVersion <$> arbitrary

newtype SomeString = S { unString :: T.Text } deriving (Eq, Show)

instance Versionable SomeString

instance Arbitrary SomeString where
  arbitrary = S . T.pack <$> resize 50 arbitrary

instance Serialize SomeString where
  put (S t) = put (encodeUtf8 t)
  get       = S . decodeUtf8 <$> get

instance (Arbitrary a, Serialize a) => Arbitrary (StoredEvent a) where
  arbitrary = StoredEvent <$>
    arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


-- | An in-memory storage that fails when serialized input's "checksum" is odd
newtype FallibleStorage = FallibleStorage { mem :: TVar [ BS.ByteString ] }

instance Storage STM FallibleStorage where
  persist FallibleStorage{..} (Store x k)    = lift (handleStore x) >>= k
    where
      handleStore v = do
        let bs   =  runPut $ put v
            csum = (sum $ BS.unpack $ bs) `mod` 127
        if csum `mod` 17 /= 0
          then modifyTVar' mem (bs:) >> return (Right v)
          else retry

  persist FallibleStorage{..} (Load Offset{..} Count{..} g k) = lift (checkErrors . map g <$> readTVar mem) >>= k
    where
      checkErrors xs = case partitionEithers xs of
        ([],rs)   -> Right $ reverse $ take (fromIntegral count) $ drop (fromIntegral offset) $ rs
        ((e:_),_) -> Left  $ IOError $ T.pack e

  persist FallibleStorage{..} (Reset k)      = lift (writeTVar mem [] >> return (Right ())) >>= k


makeMemoryStore :: STM FallibleStorage
makeMemoryStore = FallibleStorage <$> newTVar []

readMemoryStore :: FallibleStorage -> STM [ BS.ByteString ]
readMemoryStore = readTVar . mem
