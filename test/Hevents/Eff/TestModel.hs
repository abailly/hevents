module Hevents.Eff.TestModel  where

import           Data.Typeable
import           Hevents.Eff
import           Test.QuickCheck

newtype TestModel = TestModel { val :: Int } deriving Typeable

instance Model TestModel  where
  data Command TestModel = Inc Int | Dec Int deriving (Show)
  data Event TestModel   = Added Int deriving (Show)
  data Error TestModel   = OutOfBounds

  init = TestModel 0

  TestModel n `act` Inc k = if n + k > 100
                            then KO OutOfBounds
                            else OK (Added k)

  TestModel n `act` Dec k = if n - k < 0
                            then KO OutOfBounds
                            else OK $ Added (-k)

  TestModel n `apply` Added k = TestModel (n + k)

instance Arbitrary (Command TestModel) where
  arbitrary = oneof [ Inc <$> choose (1,10)
                    , Dec <$> choose (1,10)
                    ]
