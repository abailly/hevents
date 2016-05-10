{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
module Hevents.Eff.Demo where

-- * Imports, stuff to make the compiler happy

import           Control.Category
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import qualified Control.Eff                as E
import           Control.Eff.Exception
import           Control.Eff.Lift           as E hiding (lift)
import           Control.Exception          (finally)
import           Control.Monad.Except
import qualified Control.Monad.State        as ST
import           Control.Monad.Trans.Either
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Builder    as BS
import           Data.Either                (rights)
import           Data.Proxy
import           Data.Serialize             (Serialize, get, put, runGet)
import           Data.Typeable
import           Data.Void
import           Hevents.Eff                as W
import           Network.Wai.Handler.Warp   as W
import           Prelude                    hiding (init, (.))
import           Servant
import           Servant.Client
import           Test.Hspec
import           Test.QuickCheck            as Q
import           Test.QuickCheck.Monadic    as Q

-- * First property to drive our counter model
counterSpec :: Spec
counterSpec = describe "Counter model" $ do
  it "should apply result of commands given it respects bounds" $ property $ prop_shouldActAndApplyCommandsRespectingBounds

-- * The commands that can act on the model
instance Arbitrary (Command Counter) where
  arbitrary = oneof [ Increment <$> number
                    , Decrement <$> number
                    ]
    where
      number = choose (0,10)

-- * First property, test all commands against pristine model
prop_shouldActAndApplyCommandsRespectingBounds :: Command Counter -> Bool
prop_shouldActAndApplyCommandsRespectingBounds c@(Increment i) = let OK result = init `act` c
                                                                 in init `apply` result == Counter i
prop_shouldActAndApplyCommandsRespectingBounds c@(Decrement i) = let counter = Counter 10
                                                                     OK result = counter `act` c
                                                                 in counter `apply` result == Counter (10 - i)

-- * The Counter model

instance Model Counter where
  data Command Counter = Increment Int | Decrement Int deriving (Eq,Show)
  data Event Counter = Added Int deriving (Eq,Show)
  data Error Counter = OutOfBounds deriving (Eq, Show)

  init = 0

  Counter n `act`   Increment i = if (n + i) > 100
                                  then KO OutOfBounds
                                  else OK $ Added i
  Counter n `act`   Decrement i = if (n - i) < 0
                                  then KO OutOfBounds
                                  else OK $ Added (-i)
  Counter n `apply` Added a     = Counter (n + a)

newtype Counter = Counter { counter :: Int } deriving (Eq, Show, Num)
