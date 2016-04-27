An effect for persistently storing some events. `Store` effect is abstract in the sense that some parts of the
interpretation of the "requests" for this effect are left to low-level instances of `Storage`.  

> module Hevents.Eff.Store where
> 
> import Data.ByteString as BS
> import Data.Serialize
> import           System.Clock
> import Data.Aeson as A
> 
> newtype EventVersion = EventVersion { unVersion :: Int } deriving (Show, Eq)
>
> currentVersion :: EventVersion
> currentVersion = EventVersion 1
> 
> newtype SHA1 = SHA1 { unSha1 :: ByteString } deriving (Show, Eq)
>
> currentSha1 :: SHA1
> currentSha1 = SHA1 $ "000000000000000000"
>

A "unique" identifier for events which is a 128-bit value.

> newtype EventId = EventId { unId :: ByteString } deriving (Show, Eq)
> 
> class (Monad m) => MonadStore m where
>   store :: ToJSON a => a -> m ()
>   load :: FromJSON a => m x -> IO [a]
> 

A `StoredEvent` is a basic unit of storage. 

> data StoredEvent a =
>   (Serialize a) => StoredEvent { eventVersion :: EventVersion  -- ^Version of this event, useful to support migration and graceful upgrades of events
>                               , eventDate    :: TimeSpec      -- ^Timestamp for this event, a pair of (seconds,ns) since Epoch
>                               , eventSHA1    :: SHA1          -- ^Current source code version at time of event
>                               , eventId      :: EventId       -- ^Unique identifier for this event
>                               , event        :: a
>                               }
> 
> instance Show s => Show (StoredEvent s) where
>   show (StoredEvent v d s i ev) = "StoredEvent " ++ show v ++ " " ++ show d ++ " " ++ show s ++ " " ++ show i ++ " " ++ show ev
> 
> instance Eq s => Eq (StoredEvent s) where
>   (StoredEvent v d s i ev) == (StoredEvent v' d' s' i' ev') =  v == v' && d == d' && s == s' && ev == ev' && i == i'
> 
> instance (Serialize a ) => Serialize (StoredEvent a) where
>   put StoredEvent{..} = do
>     put           $ unVersion eventVersion
>     put           $ sec $ eventDate
>     put           $ nsec $ eventDate
>     putByteString $ unSha1 eventSHA1
>     putByteString $ unId eventId
>     let payload = runPut $ put event
>         len     = BS.length payload
>     putWord64le   (fromIntegral len)
>     putByteString payload
>
>   get = do
>     v <- EventVersion <$> get
>     d <- TimeSpec <$> get <*> get
>     s <- SHA1 <$> getByteString 20
>     i <- EventId <$> getByteString 16
>     l <- fromIntegral <$> getWord64le
>     p <- getByteString l
>     case runGet get p of
>       Right val -> return $ StoredEvent v d s i val
>       Left  err -> fail err
>     

