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
import           Test.Hspec
import           Test.QuickCheck          as Q
import           Test.QuickCheck.Monadic  as Q

prop_modelNeverGoesOutOfBounds :: [Command TestModel] -> Property
prop_modelNeverGoesOutOfBounds commands = monadicIO $ do
  let i :: Eff (State TestModel :> r) (TVar TestModel)
      i = do
        m <- S.makeState
        mapM_ (applyCommand m) commands
        return m

  m  <- Q.run $ atomically $ runLift (runState i) >>= readTVar
  assert $ val m >= 0 && val m <= 100


prop_stateSerializesConcurrentWrites :: [[Command TestModel]] -> Property
prop_stateSerializesConcurrentWrites commands = monadicIO $ do
  let i :: Eff (State TestModel :> r) (TVar TestModel)
      i = S.makeState

      c :: TVar TestModel -> [ Command TestModel ] -> Eff (State TestModel :> r) [ Event TestModel ]
      c m coms = rights <$> mapM (applyCommand m) coms

      added (Added k) = k

  m   <- Q.run $ atomically $ runLift (runState i)
  evs <- concat <$> Q.run (mapConcurrently (atomically . runLift . runState . c m) commands)
  v   <- Q.run $ readTVarIO m

  assert $ val v == sum (map added evs)

prop_allowMixingTwoModels :: [ Command TestModel ] -> [ Command TestModel2 ] -> Property
prop_allowMixingTwoModels commands1 commands2 = monadicIO $ do
  let alternateCommands m1 m2 []       []       = return (m1,m2)
      alternateCommands m1 m2 (c1:c1s) []       = applyCommand m1 c1 >> alternateCommands m1 m2 c1s []
      alternateCommands m1 m2 []       (c2:c2s) = applyCommand m2 c2 >> alternateCommands m1 m2 [] c2s
      alternateCommands m1 m2 (c1:c1s) (c2:c2s) = applyCommand m2 c2 >> applyCommand m1 c1 >> alternateCommands m1 m2 c1s c2s

      initAndCommands :: Eff (State TestModel :> State TestModel2 :> Lift STM :> Void) (TVar TestModel, TVar TestModel2)
      initAndCommands = do
        m1 <- S.makeState
        m2 <- S.makeState
        alternateCommands m1 m2 commands1 commands2

  (m'1,m'2) <- Q.run $ atomically $ runLift $ runState $ runState $ initAndCommands
  (v1,v2)   <- Q.run $ do
      x <- readTVarIO m'1
      y <- readTVarIO m'2
      return (x,y)

  assert $ val v1 >= 0 && val v1 <= 100 && val2 v2 >= 1.0 && val2 v2 <= 1000000.0


spec :: Spec
spec = describe "State Effect" $ do

  it "should preserve invariant of Model" $ property $ prop_modelNeverGoesOutOfBounds

  it "should serialize concurrent writes to Model" $ property $ prop_stateSerializesConcurrentWrites

  it "should allow mixing different Model instances" $ property $ prop_allowMixingTwoModels


