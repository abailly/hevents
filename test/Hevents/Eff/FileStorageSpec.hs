module Hevents.Eff.FileStorageSpec(spec) where

import           Control.Concurrent.STM
import           Control.Eff.Lift
import           Hevents.Eff
import           Hevents.Eff.TestStore   hiding (makeMemoryStore)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic as Q

prop_storeAndLoadSerializableEvents :: [SomeString] -> Property
prop_storeAndLoadSerializableEvents s = monadicIO $ do
  st <- Q.run $ openFileStorage "test.store"
  Right loaded <- Q.run $ atomically . runLift . runStore st $ mapM_ store s >> load 0 (Count $ fromIntegral (length s))
  Q.run $ print loaded
  _ <- Q.run $ closeFileStorage st

  assert $ s == loaded

spec :: Spec
spec = describe "File Storage" $ do

  it "should store and reload provided events" $ property $ prop_storeAndLoadSerializableEvents
