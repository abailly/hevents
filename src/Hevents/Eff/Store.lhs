An effect for persistently storing events to an underlying stream. `Store` effect is abstract in the sense that some parts of the
interpretation of the "requests" for this effect are left to low-level instances of `Storage`.  

> module Hevents.Eff.Store(module Hevents.Eff.Store.Events) where
> 
> import Data.Serialize
> import Data.ByteString(ByteString)
> import Data.Text(Text)
> import Control.Eff
> import Data.Either
> import Control.Eff.Lift
> import Control.Concurrent.STM
> import Hevents.Eff.Store.Events
> import Data.Int
> 
> newtype Offset = Offset { offset :: Int64 } deriving (Eq, Ord, Show, Read, Serialize)
> newtype Count  = Count { count :: Int64 } deriving (Eq, Ord, Show, Read, Serialize)
>
> countAll :: Count
> countAll = Count (-1)
>
> data StoreError = IOError { reason :: !Text } deriving (Show)
>

A class for low-level implementation details of storage operations.

> class Storage s where
>   persist :: (SetMember Lift (Lift STM) (Store :> r)) =>  s -> Store (Eff (Store :> r) a) -> Eff (Store :> r) a
>
> newtype MemoryStorage = MemoryStorage { mem :: TVar [ ByteString ] } 
>
> instance Storage MemoryStorage where
>   persist MemoryStorage{..} (Store x k)    = lift (modifyTVar' mem (runPut (put x):) >> return Nothing) >>= k
>   persist MemoryStorage{..} (Load o c g k) = lift (Right . reverse . rights . map g <$> readTVar mem) >>= k
>   persist MemoryStorage{..} (Reset k)      = undefined
>
>
> type Getter a = ByteString -> Either String a

> data Store a where

Store a serializable value in the underlying persistent storage appending it to existing events stream.

>   Store :: (Serialize x) => x                          ->  (Maybe StoreError      -> a) -> Store a

Load a potentially partial stream from underlying storage.

>   Load  :: (Serialize x) => Offset -> Count ->  Getter x -> (Either StoreError [x] -> a) -> Store a

Reset the event stream discarding all previous events.

>   Reset ::                                   (Maybe StoreError      -> a) -> Store a

Usual boilerplate to turn `Store` in Functor and create Free monad from constructors...

> instance Functor Store where
>   f `fmap` (Store x k)    = Store x (f . k)
>   f `fmap` (Load o c g k) = Load o c g (f . k)
>   f `fmap` (Reset k)      = Reset (f . k)
> 
> store :: (Member Store r, Serialize x)
>         => x -> Eff r (Maybe StoreError)
> store x = send $ inj $ Store x id
> 
> load :: (Member Store r, Serialize x) => Offset -> Count -> Getter x -> Eff r (Either StoreError [x])
> load off cnt g = send $ inj $ Load off cnt g id
> 
> reset :: (Member Store r) => Eff r (Maybe StoreError)
> reset  = send $ inj $ Reset id

> runStore :: (SetMember Lift (Lift STM) (Store :> r), Storage s) => s -> Eff (Store :> r) w -> Eff r w
> runStore s = freeMap return (\ u -> handleRelay u (runStore s) (runStore s . persist s))

