{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
module Hevents.Eff.WebServerSpec
       where

import qualified Control.Eff                as E
import           Control.Eff.Lift           as E
import           Control.Monad.Trans        (liftIO)
import           Control.Monad.Trans.Either
import           Data.Proxy
import qualified Hevents.Eff                as W
import           Network.Wai
import           Servant                    as S
import           Servant.Client
import           Test.Hspec

type TestApi = "foo" :> Get '[JSON] Int

fooAPI :: Proxy TestApi
fooAPI = Proxy

fooHandler :: Server TestApi
fooHandler = return 42

spec :: Spec
spec = describe "Web Server Effect" $ do

  it "should answer HTTP requests" $ do
    let web :: E.Eff (W.WebServer TestApi E.:> r) Application
        web = W.serve 8081 fooAPI fooHandler

    _ <- liftIO $ runLift $ W.runWebServer $ web

    Right n <- liftIO $ runEitherT $ client fooAPI (BaseUrl Http "localhost" 8081)

    n `shouldBe` 42




