{-# LANGUAGE ScopedTypeVariables #-}
module Hevents.Eff.EffSpec(spec) where

import           Control.Concurrent.STM
import           Control.Eff             as E
import           Control.Eff.Lift        as E
import           Control.Monad           ((<=<))
import           Data.Either
import           Data.Serialize
import           Hevents.Eff             as S
import           Hevents.Eff.TestModel
import           Hevents.Eff.TestStore   as T
import           Test.Hspec
import           Test.QuickCheck         as Q
import           Test.QuickCheck.Monadic as Q

prop_combineStateAndStorage :: [ Command TestModel ] -> Property
prop_combineStateAndStorage commands = Q.monadicIO $ do
  storage <- initialiseStorage
  let
    asDBError OutOfBounds = IOError "out of bounds"
    acts :: Eff (State TestModel :> Store :> r) (TVar TestModel)
    acts = do
      m <- S.makeState
      mapM_ (either (return . Left . asDBError) store <=< applyCommand m) commands
      return m
  m <- Q.run $ atomically $ (runLift . runStore storage . runState) acts >>= readTVar
  stored :: [ Event TestModel ] <- Q.run $ atomically $ (rights . map (runGet get)) <$> readMemoryStore storage

  Q.run $ print stored
  assert $ val m >= 0 && val m <= 100

  where
    initialiseStorage = Q.run $ atomically $ T.makeMemoryStore

spec :: Spec
spec = describe "Combined State & Store Effect" $ do

  it "should abort state update when underlying storage fails" $ property $ prop_combineStateAndStorage


