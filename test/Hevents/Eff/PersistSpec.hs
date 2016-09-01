{-# LANGUAGE ScopedTypeVariables #-}
module Hevents.Eff.PersistSpec(spec) where

import           Control.Concurrent.Async
import           Control.Eff              as E
import           Control.Eff.Lift         as E
import           Control.Exception        (bracket)
import           Data.Either
import           Data.Functor
import           Data.IORef
import           Hevents.Eff              as S
import           Hevents.Eff.TestModel
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

  (v, evs) <- Q.run $ withStore $ \ st -> do
    void $ resetStore st
    m <- makePersist init st systemError
    evs <- concat <$> mapConcurrently (runLift . runState m . c) commands
    v   <- readIORef (state m)
    return (v, evs)

  LoadSucceed evs' <- Q.run $ withStore readStore

  assert $ val v == sum (map added evs)

  -- We check all events returned from actions are stored but they may be in different orders
  -- although all command execution and writes are serialized, it is possible the events be
  -- returned to this test thread in different orders
  assert $ all (`elem` evs') evs && all (`elem` evs) evs'

withStore :: (FileStorage -> IO a) -> IO a
withStore = bracket (openFileStorage "test.store") closeFileStorage

spec :: Spec
spec = describe "State Effect" $ do

  it "should serialize concurrent writes to Model" $ property $ prop_persistentStateSerializesConcurrentWrites


