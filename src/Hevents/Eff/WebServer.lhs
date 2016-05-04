> module Hevents.Eff.WebServer where
>
> import Servant
> import Control.Category
> import Prelude hiding ((.))
> import Control.Monad.Trans.Either
> import Network.Wai.Handler.Warp as W
> import Control.Eff
> import Control.Concurrent.Async
> import Servant.Server.Internal.Enter

Extend a *natural transformation* from some effectful computation to IO into the `EitherT` monad expected by servant.

> effToHandle :: (Eff r :~> IO) -> (Eff r :~> EitherT ServantErr IO)
> effToHandle = (liftNat .)
>

Some type aliases to simplify things...

> type EffServer api r = ServerT api (Eff r)
> type EffToServant r = Eff r :~> EitherT ServantErr IO

Serve given API with witness `p` implemented by handler `hdl` and  starting server on given `port`.
The actual server is built by applying the given transformation `eff` to build an actual `Server api` handler.

> runWebServer :: (HasServer api, Enter (EffServer api r) (EffToServant r) (Server api))
>              => Port -> Proxy api -> (Eff r :~> IO) -> ServerT api (Eff r) -> IO (Async ())
> runWebServer port p eff hdl = async $  W.run port $ serve p hdler
>   where
>     hdler = enter (effToHandle eff) hdl


