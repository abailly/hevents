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
import           Control.Monad.Except
import           Data.Functor               (void)
import           Data.Proxy
import           Data.Typeable
import           Data.Void
import           Hevents.Eff                as W
import           Hevents.Eff.TestModel
import           Prelude                    hiding ((.))
import           Servant
import           Servant.Client
import           Network.HTTP.Client        (newManager, defaultManagerSettings)
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
handler n = do
  r <- applyCommand (Inc n)
  void $ either (return . Left . asDBError) store r
  val <$> getState

spec :: Spec
spec = describe "Web Server Effect" $ do

  it "should serve HTTP requests given some effectful function" $ do
    (model, storage) <- prepareContext

    mgr <- newManager defaultManagerSettings

    s <- W.runWebServer 8082 testAPI (effect storage model) handler
    
    n <- runExceptT (runClient 42 mgr baseUrl) `finally` cancel s
    
    either (error . show) (`shouldBe` 42) n

      where
        baseUrl = (BaseUrl Http "127.0.0.1" 8082 "")
        prepareContext = (,) <$>
          newTVarIO (W.init :: TestModel) <*>
          atomically W.makeMemoryStore
        runClient = client testAPI



