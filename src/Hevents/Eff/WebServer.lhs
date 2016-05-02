> {-# LANGUAGE DeriveFunctor #-}
> module Hevents.Eff.WebServer where
>
> import qualified Servant as S
> import Network.Wai.Handler.Warp as W
> import Network.Wai
> import Control.Eff
> import Control.Eff.Lift
> import Data.Typeable
> import Control.Concurrent.Async
>

A `WebServer` is parameterized by the type `s` of the API it is serving. 

> data WebServer s a where
>

Build a server for given API signature `s` to be run over given `Port`.

>   Serve :: Port -> S.Proxy s -> S.Server s -> (Application -> a) -> WebServer s a
>   deriving (Functor, Typeable)
>   

> serve :: (Member (WebServer api) r, Typeable api)
>         => Port -> S.Proxy api -> S.Server api -> Eff r Application
> serve port p api = send $ inj $ Serve port p api id
> 

Interprets the `Serve` command by running the given server. 

> runWebServer :: (SetMember Lift (Lift IO) r, Typeable s, S.HasServer s) => Eff (WebServer s :> r) w -> Eff r w
> runWebServer = freeMap return (\ u -> handleRelay u runWebServer interpret)
>   where
>     interpret (Serve port proxy api k) = lift start >>= runWebServer . k
>       where start = let app = S.serve proxy api
>                     in async (W.run port app) >> return app

