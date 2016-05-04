{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
module Hevents.Eff.WebServerSpec
       where

import           Control.Category
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import qualified Control.Eff                as E
import           Control.Eff.Lift           as E hiding (lift)
import           Control.Exception
import           Control.Monad.Trans        (liftIO)
import           Control.Monad.Trans.Either
import           Data.Proxy
import           Data.Typeable
import           Data.Void
import           Hevents.Eff                as W
import           Hevents.Eff.TestModel
import           Prelude                    hiding ((.))
import           Servant
import           Servant.Client
import           Test.Hspec


type TestApi = "inc" :> Capture "increment" Int :> Get '[JSON] Int

testAPI :: Proxy TestApi
testAPI = Proxy

effect :: (Typeable m, Storage STM s, Registrar STM m reg)
              => s -> reg
              -> E.Eff (State m E.:> Store E.:> Lift STM E.:> Void) :~> IO
effect s m = Nat $ atomically . runSync .  W.runStore s . W.runState m

asDBError :: Error TestModel -> StoreError
asDBError OutOfBounds = IOError "out of bounds"

handler :: Int -> E.Eff (State TestModel E.:> Store E.:> r) Int
handler n = applyCommand (Inc n) >>= either (return . Left . asDBError) store >> getState >>= return . val

spec :: Spec
spec = describe "Web Server Effect" $ do

  it "should serve HTTP requests given some effectful function" $ do
    model <- newTVarIO (W.init :: TestModel)
    storage <- liftIO $ atomically $ W.makeMemoryStore

    s <- liftIO $ W.runWebServer 8082 testAPI (effect storage model) handler

    n <- liftIO $ runEitherT (client testAPI (BaseUrl Http "localhost" 8082) 42) `finally` cancel s

    either (error . show) (`shouldBe` 42) n






