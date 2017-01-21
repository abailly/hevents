> {-# LANGUAGE ImplicitParams      #-}

Description of `StoredEvent`s which defines how arbitrary `Serializable` events are stored in underlying
storage engine.

> module Hevents.Eff.Store.Events where
> 
> import qualified Data.ByteString as BS
> import Data.Serialize
> import           System.Clock
> 
> 
> newtype Version = Version { version :: Int } deriving (Show, Eq, Num)
>
> instance Serialize Version where
>   put (Version v) = put v
>   get             = Version <$> get
> 
> defaultVersion :: Version
> defaultVersion = Version 1
>
> class (Serialize s) => Versionable s where
>   write :: Version -> s -> BS.ByteString
>   write _ = runPut . put
>   read :: Version -> BS.ByteString -> Either String s
>   read _ = runGet get
> 
> instance Versionable () where
>   write = undefined
>   read = undefined

> newtype SHA1 = SHA1 { unSha1 :: BS.ByteString } deriving (Show, Eq)
>
> defaultSha1 :: SHA1
> defaultSha1 = SHA1 $ BS.replicate 20 0
>

A `StoredEvent` is a basic unit of storage. 

> data StoredEvent a =
>   (Serialize a) => StoredEvent { eventVersion :: Version  -- ^Version of this event, useful to support migration and graceful upgrades of events
>                               , eventDate    :: TimeSpec      -- ^Timestamp for this event, a pair of (seconds,ns) since Epoch
>                               , eventSHA1    :: SHA1          -- ^Current source code version at time of event
>                               , event        :: a
>                               }
> 
> instance Show s => Show (StoredEvent s) where
>   show (StoredEvent v d s ev) = "StoredEvent " ++ show v ++ " " ++ show d ++ " " ++ show s ++ " " ++ show ev
> 
> instance Eq s => Eq (StoredEvent s) where
>   (StoredEvent v d s ev) == (StoredEvent v' d' s' ev') =  v == v' && d == d' && s == s' && ev == ev'
> 
> instance (Serialize a ) => Serialize (StoredEvent a) where
>   put StoredEvent{..} = do
>     put           $ version eventVersion
>     put           $ sec $ eventDate
>     put           $ nsec $ eventDate
>     putByteString $ unSha1 eventSHA1
>     let payload = runPut $ put event
>         len     = BS.length payload
>     putWord64le   (fromIntegral len)
>     putByteString payload
>
>   get = do
>     v <- Version <$> get
>     d <- TimeSpec <$> get <*> get
>     s <- SHA1 <$> getByteString 20
>     l <- fromIntegral <$> getWord64le
>     p <- getByteString l
>     case runGet get p of
>       Right val -> return $ StoredEvent v d s val
>       Left  err -> fail err
>     


Convert a serializable to ByteString for binary storage

> doStore :: (?currentVersion :: Version, Versionable s) => s -> BS.ByteString
> doStore e = let bs = write ?currentVersion e
>                 crc = 42  -- TODO compute real CRC32
>             in runPut $ do
>   putWord32be $ fromIntegral (BS.length bs + 4 + 1)
>   putWord8 (fromIntegral $ version ?currentVersion)
>   putWord32be crc
>   putByteString bs
