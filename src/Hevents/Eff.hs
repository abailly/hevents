{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
module Hevents.Eff(module Hevents.Eff.Model,
                   module Hevents.Eff.State,
                   module Hevents.Eff.State.InMemory,
                   module Hevents.Eff.Store,
                   module Hevents.Eff.Sync,
                   module Hevents.Eff.WebServer,
                   module Hevents.Eff.Store.MemoryStorage,
                   module Hevents.Eff.Store.FileStorage,
                   effect)
                  where

import Control.Eff               
import           Data.Void
import           Control.Eff.Exception
import           Control.Eff.Lift           as E hiding (lift)
import           Control.Concurrent.STM
import           Hevents.Eff.Model
import           Hevents.Eff.State
import           Hevents.Eff.State.InMemory
import           Hevents.Eff.Store
import           Hevents.Eff.Store.FileStorage
import           Hevents.Eff.Store.MemoryStorage
import           Hevents.Eff.Sync
import           Hevents.Eff.WebServer
import Data.Typeable

-- | A generic composite  `effect` made from all available effects
effect :: (Typeable m, Typeable e, Storage STM s, Registrar STM m reg)
         => s -> reg
         -> Eff (State m :> Store :> Exc e :> Lift STM :> Void) a -> IO (Either e a)
effect s m = atomically . runSync . runExc . runStore s .  runState m
