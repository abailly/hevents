module Hevents.Eff.StoreSpec where

import           Control.Concurrent.STM
import           Control.Eff.Lift
import           Data.Either
import           Data.Serialize
import           Hevents.Eff.Store.MemoryStorage
import           Hevents.Eff.TestStore
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic         as Q

prop_roundtripSerialization :: StoredEvent SomeString -> Property
prop_roundtripSerialization e = let bs  = runPut $ put e
                                    res = runGet get bs
                                in counterexample ("fail to roundtrip, bytes are " ++ show bs) $
                                   case res of
                                     Right decoded -> decoded == e
                                     Left err      -> error err

prop_storeAndLoadSerializableEvents :: [SomeString] -> Property
prop_storeAndLoadSerializableEvents s = monadicIO $ do
  st <- Q.run $ atomically $ makeMemoryStore
  Right loaded <- Q.run $ atomically . runLift . runStore st $ mapM_ store s >> load 0 (Count $ fromIntegral (length s))

  -- read actual content of memory storage and decode it to check data is actually "persisted"
  stored <- Q.run $ atomically $ (reverse . rights . map (runGet get)) <$> readTVar (mem st)

  assert $ s == loaded && s == stored

spec :: Spec
spec = describe "Store Effect" $ do

  it "should roundtrip event given payload is serializable" $ property $ prop_roundtripSerialization

  it "should store all serializable events in given storage" $ property $ prop_storeAndLoadSerializableEvents
