{-# LANGUAGE ScopedTypeVariables #-}
module Hevents.Eff.StateSpec(spec) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Eff              as E
import           Control.Eff.Lift         as E
import           Data.Either
import           Data.Void
import           Hevents.Eff              as S
import           Hevents.Eff.TestModel
import           Hevents.Eff.TestModel2
import           Prelude                  hiding (init)
import           Test.Hspec
import           Test.QuickCheck          as Q
import           Test.QuickCheck.Monadic  as Q

prop_modelNeverGoesOutOfBounds :: [Command TestModel] -> Property
prop_modelNeverGoesOutOfBounds commands = monadicIO $ do
  let i :: Eff (State TestModel :> r) TestModel
      i = do
        mapM_ applyCommand commands
        getState

  v <- Q.run $ newTVarIO (init :: TestModel)
  m  <- Q.run $ atomically $ runLift (runState v i)
  assert $ val m >= 0 && val m <= 100


prop_stateSerializesConcurrentWrites :: [[Command TestModel]] -> Property
prop_stateSerializesConcurrentWrites commands = monadicIO $ do
  let
      c :: [ Command TestModel ] -> Eff (State TestModel :> r) [ Event TestModel ]
      c coms = rights <$> mapM applyCommand coms

      added (Added k) = k

  m <- Q.run $ newTVarIO init
  evs <- concat <$> Q.run (mapConcurrently (atomically . runLift . runState m . c) commands)
  v   <- Q.run $ readTVarIO m

  assert $ val v == sum (map added evs)

prop_allowMixingTwoModels :: [ Command TestModel ] -> [ Command TestModel2 ] -> Property
prop_allowMixingTwoModels commands1 commands2 = monadicIO $ do
  let alternateCommands []       []       = (,) <$> getState <*> getState
      alternateCommands (c1:c1s) []       = applyCommand c1 >> alternateCommands c1s []
      alternateCommands []       (c2:c2s) = applyCommand c2 >> alternateCommands [] c2s
      alternateCommands (c1:c1s) (c2:c2s) = applyCommand c2 >> applyCommand c1 >> alternateCommands c1s c2s

      initAndCommands :: Eff (State TestModel :> State TestModel2 :> Lift STM :> Void) (TestModel, TestModel2)
      initAndCommands = do
        alternateCommands commands1 commands2

  (m1,m2) <- Q.run $ (,) <$> newTVarIO (init :: TestModel) <*> newTVarIO (init :: TestModel2)
  (v1,v2) <- Q.run $ atomically $ runLift $ runState m2 $ runState m1 $ initAndCommands

  assert $ val v1 >= 0 && val v1 <= 100 && val2 v2 >= 1.0 && val2 v2 <= 1000000.0


spec :: Spec
spec = describe "State Effect" $ do

  it "should preserve invariant of Model" $ property $ prop_modelNeverGoesOutOfBounds

  it "should serialize concurrent writes to Model" $ property $ prop_stateSerializesConcurrentWrites

  it "should allow mixing different Model instances" $ property $ prop_allowMixingTwoModels


