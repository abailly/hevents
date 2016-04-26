Provides machinery to maintain and update `State` for some 

> module Hevents.State where
>
> import Hevents.Model
> import Hevents.Store
> import Data.Aeson
> import Control.Monad.Except
> import Control.Monad.Reader
> import Control.Concurrent.STM
> import System.Clock
> 
> class (Model a, EventClassifier s, ToJSON (EventType s)) => CommandExecutor a s | s -> a where
>   getView :: s -> a
>   getEventType :: Event a -> EventType s
>   setView :: (a -> s -> s)
> 
> applyCommand :: (MonadIO m, MonadStore m, MonadError (Error a) m, MonadReader (TVar s) m, ToJSON (Event a), CommandExecutor a s)
>                  => Command a
>                  -> m (Event a)
> applyCommand command = do
>   v <- ask
>   (ts, ev) <- liftIO $ do
>     ts <- getTime Realtime
>     ev <- atomically $ actAndApply v command
>     return (ts, ev)
>   let stored e etype = store (makeStoredEvent etype ts e)
>   case ev of
>    Right (e, etype) -> stored e etype >> return e
>    Left l  -> throwError l
> 
> actAndApply :: (CommandExecutor a s) => TVar s -> Command a -> STM (Either (Error a) (Event a, EventType s))
> actAndApply v command = do
>   s <- readTVar v
>   let view = getView s
>       modifyState (KO er) = return $ Left er
>       modifyState (OK ev) =  do
>           let newView = view `apply` ev
>           modifyTVar' v (setView newView)
>           return $ Right (ev, getEventType ev)
>   modifyState (view `act` command)

