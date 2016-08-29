module Hevents.Eff.TestModel2  where

import           Data.Typeable
import           Hevents.Eff
import           Test.QuickCheck

newtype TestModel2 = TestModel2 { val2 :: Double } deriving Typeable

data CTestModel2 = Mul Double | Div Double deriving (Show)
data ETestModel2   = Factor Double deriving (Show)
data ErTestModel2   = OutOfBounds

type instance Event TestModel2 = ETestModel2
type instance Command TestModel2 = CTestModel2
type instance Error TestModel2 = ErTestModel2

instance Model TestModel2  where

  init = TestModel2 1.0

  TestModel2 x `act` Mul y = if x * y > 1000000.0
                             then KO OutOfBounds
                             else OK (Factor y)

  TestModel2 x `act` Div y = if x / y < 1.0
                            then KO OutOfBounds
                            else OK $ Factor (1 / y)

  TestModel2 x `apply` Factor y = TestModel2 (x * y)

instance Arbitrary CTestModel2 where
  arbitrary = oneof [ Mul <$> choose (3,30)
                    , Div <$> choose (3,30)
                    ]

