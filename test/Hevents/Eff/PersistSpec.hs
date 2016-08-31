{-# LANGUAGE ScopedTypeVariables #-}
module Hevents.Eff.PersistSpec(spec) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Eff              as E
import           Control.Eff.Lift         as E
import           Control.Exception        (bracket)
import           Data.Either
import           Data.IORef
import           Data.Void
import           Hevents.Eff              as S
import           Hevents.Eff.TestModel
import           Hevents.Eff.TestModel2
import           Prelude                  hiding (init)
import           Test.Hspec
import           Test.QuickCheck          as Q
import           Test.QuickCheck.Monadic  as Q

systemError :: StoreError -> Error TestModel
systemError (IOError e) = SystemError e

prop_persistentStateSerializesConcurrentWrites :: [[Command TestModel]] -> Property
prop_persistentStateSerializesConcurrentWrites commands = monadicIO $ do
  let
      c :: [ Command TestModel ] -> Eff (State TestModel :> r) [ Event TestModel ]
      c coms = rights <$> mapM applyCommand coms

      added (Added k) = k

  (v, evs) <- Q.run $ withStore $ \ store -> do
    m <- makePersist init store systemError
    evs <- concat <$> mapConcurrently (runLift . runState m . c) commands
    v   <- readIORef (state m)
    return (v, evs)

  assert $ val v == sum (map added evs)

withStore :: (FileStorage -> IO a) -> IO a
withStore = bracket (openFileStorage "test.store") closeFileStorage

spec :: Spec
spec = describe "State Effect" $ do

  it "should serialize concurrent writes to Model" $ property $ prop_persistentStateSerializesConcurrentWrites


