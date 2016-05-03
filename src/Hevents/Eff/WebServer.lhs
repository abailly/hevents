> {-# LANGUAGE DeriveFunctor #-}
> module Hevents.Eff.WebServer where
>
> import qualified Servant as S
> import Network.Wai.Handler.Warp as W
> import Control.Eff
> import Data.Typeable
> import Control.Concurrent.Async
> import Debug.Trace
>

A Type for endpoints inputing values of type `a` and returning values of type `b`.

> data Endpoint a b
>

A `WebServer` is parameterized by the type `r` of effects it contains.

> data WebServer r where
>

Build a server for given API signature `s` to be run over given `Port`.

>   Get :: Endpoint a b -> (a -> Eff r b) -> WebServer r
>   deriving (Typeable)
> 


> get :: Endpoint a b -> (a -> Eff r b) -> WebServer r
> get = Get
> 

Interprets the `Serve` command by running the given server. 

> runWebServer :: (S.HasServer api) => Port -> (Proxy api) -> S.Server api -> IO (Async ())
> runWebServer port p server = async $ trace "started server" $ W.run port (S.serve p server) 


