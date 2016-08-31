module Hevents.Eff.TestModel  where

import           Data.Serialize
import           Data.Text
import           Data.Typeable
import           Hevents.Eff
import           Test.QuickCheck

newtype TestModel = TestModel { val :: Int } deriving (Eq, Show, Typeable)

data CTestModel = Inc Int | Dec Int deriving (Show)
data ETestModel   = Added Int deriving (Show, Eq)
data ErTestModel   = OutOfBounds
                   | SystemError Text

type instance Event TestModel = ETestModel
type instance Command TestModel = CTestModel
type instance Error TestModel = ErTestModel

instance Model TestModel  where
  init = TestModel 0

  TestModel n `act` Inc k = if n + k > 100
                            then KO OutOfBounds
                            else OK (Added k)

  TestModel n `act` Dec k = if n - k < 0
                            then KO OutOfBounds
                            else OK $ Added (-k)

  TestModel n `apply` Added k = TestModel (n + k)

instance Arbitrary CTestModel where
  arbitrary = oneof [ Inc <$> choose (1,10)
                    , Dec <$> choose (1,10)
                    ]

instance Serialize ETestModel where
  put (Added i) = put i
  get           = Added <$> get

instance Versionable ETestModel

instance Serialize ErTestModel where
  -- we don't really care about how this is serialized..
  put _ = put ("Error" :: String)
  get   = pure OutOfBounds

instance Versionable ErTestModel

