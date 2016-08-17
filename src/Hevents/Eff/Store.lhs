An effect for persistently storing events to an underlying stream. `Store` effect is abstract in the sense that some parts of the
interpretation of the "requests" for this effect are left to low-level instances of `Storage`.  

> module Hevents.Eff.Store(module Hevents.Eff.Store.Events,
>                          Storage(..),Reader,Store(..),StoreError(..),Count(..),Offset(..),
>                          Versionable(..), Version(..),
>                          store,load,reset,
>                          runStore) where
> 
> import Data.Serialize
> import Data.ByteString(ByteString)
> import Data.Text(Text)
> import Control.Eff
> import Control.Eff.Lift
> import Hevents.Eff.Store.Events
> import Data.Int
> 
> newtype Offset = Offset { offset :: Int64 } deriving (Eq, Ord, Show, Read, Serialize, Num)
> newtype Count  = Count { count :: Int64 } deriving (Eq, Ord, Show, Read, Serialize, Num)
>
> data StoreError = IOError { reason :: !Text } deriving (Show)
>

A class for low-level implementation details of storage operations. A `Storage` will be used by a `Store`
to *atomically* `persist` a serializable value. The persistence action is run as part of a STM transaction
with the intent that if it fails, one can `retry` or `abort` the transaction hence discard any other transactional
effects that might require storage to be successful.

> class (Serialize s) => Versionable s where
>   write :: Version -> s -> ByteString
>   write _ = runPut . put
>   read :: Version -> ByteString -> Either String s
>   read _ = runGet get
>
> instance Versionable () where
>   write = undefined
>   read = undefined
>   
> newtype Version  = Version { version :: Int } deriving (Eq,Show, Num)
> 
> class Monad m => Storage m s where
>   persist :: (SetMember Lift (Lift m) (Store :> r)) =>  s -> Store (Eff (Store :> r) a) -> Eff (Store :> r) a
>
> type Reader a = ByteString -> Either String a
>
> type StoreResult a = Either StoreError a
> 
> data Store a where

Store a serializable value in the underlying persistent storage appending it to existing events stream.

>   Store :: (Versionable x) => x                          -> (StoreResult x   -> a) -> Store a

Load a potentially partial stream from underlying storage. We need to pass the `Reader x` function as
the type of object to deserialize is packed within the constructor hence cannot be known by the
`Storage` responsible for fetching the data.

>   Load  :: (Versionable x) => Offset -> Count ->  Reader x -> (StoreResult [x] -> a) -> Store a

Reset the event stream discarding all previous events.

>   Reset ::                                              (StoreResult ()   -> a) -> Store a

Usual boilerplate to turn `Store` in Functor and create Free monad from constructors...

> instance Functor Store where
>   f `fmap` (Store x k)    = Store x (f . k)
>   f `fmap` (Load o c g k) = Load o c g (f . k)
>   f `fmap` (Reset k)      = Reset (f . k)
> 
> store :: (Member Store r, Versionable x)
>         => x -> Eff r (StoreResult x)
> store x = send $ inj $ Store x id
> 
> load :: (Member Store r, Versionable x) => Offset -> Count -> Eff r (StoreResult [x])
> load off cnt = send $ inj $ Load off cnt (runGet get) id
> 
> reset :: (Member Store r) => Eff r (StoreResult ())
> reset  = send $ inj $ Reset id

Run `Store` actions which are part of some `Union` of actions. The actual storage operations are delegated
to given `Storage` instance.

> runStore :: (SetMember Lift (Lift m) (Store :> r), Storage m s) => s -> Eff (Store :> r) w -> Eff r w
> runStore s = freeMap return (\ u -> handleRelay u (runStore s) (runStore s . persist s))

