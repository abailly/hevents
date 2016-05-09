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
import           Prelude                    hiding ((.))
import           Servant
import           Servant.Client
import           Test.Hspec
import           Test.QuickCheck

-- * Define REST Interface

type CounterApi = "counter" :> (Get '[JSON] Int
                                :<|> "increment" :> Capture "inc" Int :> Get '[JSON] Int
                                :<|> "decrement" :> Capture "dec" Int :> Get '[JSON] Int)


