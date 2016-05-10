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
