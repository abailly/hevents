{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
module Hevents.Eff.WebServerSpec
       where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import qualified Control.Eff                as E
import           Control.Eff.Lift           as E hiding (lift)
import           Control.Exception
import           Control.Monad.Trans        (lift, liftIO)
import           Control.Monad.Trans.Either
import           Data.Proxy
import           Data.Void
import           Hevents.Eff                as W
import           Hevents.Eff.TestModel
import           Servant
import           Servant.Client
import           Test.Hspec


type TestApi = "test" :> Capture "userid" Int :> Get '[JSON] Int

testAPI :: Proxy TestApi
testAPI = Proxy

spec :: Spec
spec = describe "Web Server Effect" $ do

  it "should serve HTTP requests given some effectful function" $ do
    let
--      effToHandle :: TVar TestModel -> E.Eff (State TestModel E.:> Lift STM E.:> Void) a -> Server a
      effToHandle s m = Nat $ \ e -> (lift $ atomically $ runLift $  W.runStore s $ W.runState m e) >>= right

      asDBError OutOfBounds = IOError "out of bounds"

      hdl :: Int -> E.Eff (State TestModel E.:> Store E.:> r) Int
      hdl n = applyCommand (Inc n) >>= either (return . Left . asDBError) store >> getState >>= return . val

      appServer :: ServerT TestApi (E.Eff (State TestModel E.:> Store E.:> Lift STM E.:> Void))
      appServer = hdl

      appHandler :: MemoryStorage -> TVar TestModel -> Server TestApi
      appHandler s m = enter (effToHandle s m) appServer

    m <- newTVarIO W.init
    st <- liftIO $ atomically $ W.makeMemoryStore

    s <- liftIO $ W.runWebServer 8082 testAPI (appHandler st m)

    n <- liftIO $ runEitherT (client testAPI (BaseUrl Http "localhost" 8082) 12) `finally` cancel s

    either (error . show) (`shouldBe` 12) n






