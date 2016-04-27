module Hevents.Eff.TestStore(module Hevents.Eff.Store, SomeString(..)) where

import           Data.ByteString    as BS
import           Data.Serialize
import           Data.Text          as T
import           Data.Text.Encoding
import           Hevents.Eff.Store
import           System.Clock
import           Test.QuickCheck

instance Arbitrary EventId where
  arbitrary = EventId . BS.pack <$> vectorOf 16 arbitrary

instance Arbitrary SHA1 where
  arbitrary = SHA1 . BS.pack <$> vectorOf 20 arbitrary

instance Arbitrary TimeSpec where
  arbitrary = TimeSpec <$> arbitrary <*> arbitrary

instance Arbitrary EventVersion where
  arbitrary = EventVersion <$> arbitrary

newtype SomeString = S { unString :: Text } deriving (Eq, Show)

instance Arbitrary SomeString where
  arbitrary = S . T.pack <$> resize 50 arbitrary

instance Serialize SomeString where
  put (S t) = put (encodeUtf8 t)
  get       = S . decodeUtf8 <$> get

instance (Arbitrary a, Serialize a) => Arbitrary (StoredEvent a) where
  arbitrary = StoredEvent <$>
    arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

