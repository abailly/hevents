{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
module Hevents.Eff.Demo where

import           Control.Category
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import qualified Control.Eff                as E
import           Control.Eff.Lift           as E hiding (lift)
import           Control.Exception
import           Control.Monad              (forM)
import qualified Control.Monad.State        as ST
import           Control.Monad.Trans.Either
import           Data.Functor               (void)
import           Data.Proxy
import           Data.Serialize             (Serialize, get, put)
import           Data.Typeable
import           Data.Void
import           Hevents.Eff                as W
import           Network.Wai.Handler.Warp   as W
import           Prelude                    hiding (init, (.))
import           Servant
import           Servant.Client
import           Test.Hspec
import           Test.QuickCheck

-- * Define REST Interface

type CounterApi = "counter" :> (Get '[JSON] Int
                                :<|> "increment" :> Capture "inc" Int :> Get '[JSON] Int
                                :<|> "decrement" :> Capture "dec" Int :> Get '[JSON] Int)

counterApi :: Proxy CounterApi
counterApi = Proxy

-- * Minimal implementation

counterServer :: Server CounterApi
counterServer = getCounter
  :<|> increment
  :<|> decrement
  where
    getCounter = return 0
    increment  = const undefined
    decrement  = const undefined

-- * A Test
apiSpec :: Spec
apiSpec = describe "Counter Server" $ do
  it "should return 0 as initial state of counter" $ do
    s <- async $ W.run 8082 $ serve counterApi counterServer
    Right n <- (runEitherT $ getCounter) `finally` cancel s

    n `shouldBe` 0
      where
        getCounter :<|> _  :<|> _ = client counterApi (BaseUrl Http "localhost" 8082)

-- * The counter model
-- A bounded counter

instance Model Counter where
  data Command Counter = Increment Int | Decrement Int deriving (Eq,Show)
  data Event Counter = Added Int deriving (Eq,Show)
  data Error Counter = OutOfBounds deriving (Eq, Show)

  init = 0

  Counter _ `act`   Increment i = OK $ Added i
  Counter n `act`   Decrement i = if (n - i) < 0
                                  then KO OutOfBounds
                                  else OK $ Added (-i)
  Counter n `apply` Added a     = Counter (n + a)

newtype Counter = Counter { counter :: Int } deriving (Eq, Show, Num)

-- * Test Counter properties

instance Arbitrary (Command Counter) where
  arbitrary = oneof [ Increment <$> number
                    , Decrement <$> number
                    ]
    where
      number = choose (0,10)

prop_shouldActAndApplyCommandsRespectingBounds :: Command Counter -> Bool
prop_shouldActAndApplyCommandsRespectingBounds c@(Increment i) = let OK result = init `act` c
                                                                 in init `apply` result == Counter i
prop_shouldActAndApplyCommandsRespectingBounds c@(Decrement i) = let counter = Counter 10
                                                                     OK result = counter `act` c
                                                                 in counter `apply` result == Counter (10 - i)

prop_shouldNotApplyCommandsOverBounds :: [Command Counter] -> Bool
prop_shouldNotApplyCommandsOverBounds commands = let finalCounter = counter $ ST.execState (mapM_ updateModel commands) init
                                                 in  finalCounter >= 0 && finalCounter <= 100

counterSpec :: Spec
counterSpec = describe "Counter model" $ do
  it "should apply result of commands given it respects bounds" $ property $ prop_shouldActAndApplyCommandsRespectingBounds
  it "should not allow applying commands out of bounds"         $ property $ prop_shouldNotApplyCommandsOverBounds
