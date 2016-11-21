{-# LANGUAGE ScopedTypeVariables #-}
module Hevents.Eff.LogSpec(spec) where

import           Hevents.Eff     as S
import           Prelude         hiding (init)
import           Test.Hspec
import           Test.QuickCheck as Q

prop_collectLogInAList :: [ Int ] -> Bool
prop_collectLogInAList list = let logged = S.run $ runLogPure $ mapM logE list
                              in  snd logged  == list

spec :: Spec
spec = describe "Log Effect" $ do

  it "collect logged items in a list" $ property $ prop_collectLogInAList


