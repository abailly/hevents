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


type TestApi = "test" :> Capture "userid" Int :> Get '[JSON] Int

testAPI :: Proxy TestApi
testAPI = Proxy

effect :: (Typeable m, Storage STM s, Registrar STM m reg)
              => s -> reg
              -> E.Eff (State m E.:> Store E.:> Lift STM E.:> Void) :~> IO
effect s m = Nat $ atomically . runSync .  W.runStore s . W.runState m

effToHandle :: (E.Eff r :~>  IO) -> (E.Eff r :~> EitherT e IO)
effToHandle = (liftNat .)

spec :: Spec
spec = describe "Web Server Effect" $ do

  it "should serve HTTP requests given some effectful function" $ do
    let

      asDBError OutOfBounds = IOError "out of bounds"

      hdl :: Int -> E.Eff (State TestModel E.:> Store E.:> r) Int
      hdl n = applyCommand (Inc n) >>= either (return . Left . asDBError) store >> getState >>= return . val

      appServer :: ServerT TestApi (E.Eff (State TestModel E.:> Store E.:> Lift STM E.:> Void))
      appServer = hdl

      appHandler :: MemoryStorage -> TVar TestModel -> Server TestApi
      appHandler s m = enter (effToHandle (effect s m)) appServer

    m <- newTVarIO W.init
    st <- liftIO $ atomically $ W.makeMemoryStore

    s <- liftIO $ W.runWebServer 8082 testAPI (appHandler st m)

    n <- liftIO $ runEitherT (client testAPI (BaseUrl Http "localhost" 8082) 12) `finally` cancel s

    either (error . show) (`shouldBe` 12) n






