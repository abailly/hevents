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
import           Control.Monad.Trans.Either
import           Data.Functor               (void)
import           Data.Proxy
import           Data.Serialize             (Serialize, get, put)
import           Data.Typeable
import           Data.Void
import           Hevents.Eff                as W
import           Network.Wai.Handler.Warp   as W
import           Prelude                    hiding ((.))
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

instance Model Counter where
  data Command Counter = Increment Positive | Decrement Positive deriving (Eq,Show)
  data Event Counter = Added Int deriving (Eq,Show)
  data Error Counter = OutOfBounds deriving (Eq, Show)
