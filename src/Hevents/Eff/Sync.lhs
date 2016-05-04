Synchronizes multiple effects running in the `STM` monad to manage abortion of transaction.
The goal is to ensure the underlying STM transaction is run only once: If it `retry`s then
it is aborted and an exception is thrown.

> module Hevents.Eff.Sync(SyncException(..), runSync) where
>
> import Control.Concurrent.STM
> import Control.Eff.Lift
> import Data.Void
> import Control.Eff
> import Control.Exception
>

Type of exceptions thrown by sync computations

> data SyncException = SyncException String deriving (Eq,Show)
>
> instance Exception SyncException
> 
> runSync :: Eff (Lift STM :> Void) w -> STM w
> runSync eff = runLift eff `orElse` throwSTM (SyncException "effect failed to complete succesfully, aborting") 


