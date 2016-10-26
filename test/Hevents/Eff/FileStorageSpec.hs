module Hevents.Eff.FileStorageSpec(spec) where

import           Control.Eff.Lift
import           Control.Exception       (bracket)
import           Hevents.Eff
import           Hevents.Eff.TestStore   hiding (makeMemoryStore)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic as Q

prop_storeAndLoadSerializableEvents :: [SomeString] -> Property
prop_storeAndLoadSerializableEvents s = monadicIO $ do
  Right loaded <- Q.run $ withStorage "test.store" $ \ st -> (runLift . runStore st $ reset >> mapM store s >> load 0 (Count $ fromIntegral (length s)))
  assert $ s == loaded

spec :: Spec
spec = describe "File Storage" $ do

  -- it "test storage" $ do
  --   let strings = map S [ "foo", "bar", "baz" ]
  --   loaded <- withStore $ \ st -> (runLift . runStore st $ mapM store strings) -- >> load 0 (Count $ fromIntegral (length s))

  --   rights loaded `shouldBe` strings

  it "should store and reload provided events" $ property $ prop_storeAndLoadSerializableEvents



