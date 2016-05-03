module Hevents.Eff.State.InMemory where

import           Control.Concurrent.STM
import           Control.Eff.Lift
import           Hevents.Eff.Model      hiding (init)
import           Hevents.Eff.State

type InMemoryState m = TVar m

instance (Model m) => Registrar STM m (InMemoryState m) where
  update m (ApplyCommand c k) = lift (actAndApply m c) >>= k
  update m (GetState k)       = lift (readTVar m)      >>= k

actAndApply :: (Model m) => TVar m -> Command m -> STM (Either (Error m) (Event m))
actAndApply v command = do
  s <- readTVar v
  let modifyState (KO er) = return $ Left er
      modifyState (OK ev) =  do
        let newView = s `apply` ev
        writeTVar v newView
        return $ Right ev
  modifyState (s `act` command)
