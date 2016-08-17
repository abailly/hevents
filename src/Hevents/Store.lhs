The `Store` is responsible for persistent storage of events stemming from some `Model`. 

> module Hevents.Store where
> 
> import qualified Data.ByteString.Base64.Lazy as B64
> import           Data.ByteString.Lazy        (ByteString)
> import           Data.Char                   (isHexDigit)
> import           Data.List                   as List
> import qualified Data.Text                   as T
> import           Data.Text.Lazy              as L
> import qualified Data.Text.Lazy.Encoding     as LE
> import           Hevents.Model
> import           System.Clock                (toNanoSecs, fromNanoSecs, TimeSpec)
> import Data.Aeson as A
> import Data.String
> 
> type EventVersion = Int
> 
> class (Show (EventType c), Enum (EventType c)) => EventClassifier c where
>   data EventType c :: *
>   applyEvent :: c -> StoredEvent c -> c
> 
> currentVersion :: EventVersion
> currentVersion = 1
> 
> class (Monad m) => MonadStore m where
>   store :: ToJSON a => a -> m ()
>   load :: FromJSON a => m x -> IO [a]
> 
> makeStoredEvent ::  ToJSON (Event a)
>                    => EventType s
>                    -> TimeSpec
>                    -> Event a
>                    -> StoredEvent s
> makeStoredEvent etype ts = StoredEvent currentVersion etype ts currentSha1 . encode
> 
> data StoredEvent s = StoredEvent { eventVersion :: EventVersion   -- ^Version of this event, useful to support migration and graceful upgrades of events
>                                  , eventType    :: EventType s    -- ^Type of event, needed to properly deserialize the @event@ when needed
>                                  , eventDate    :: TimeSpec       -- ^Timestamp for this event, a pair of (seconds,ns) since Epoch
>                                  , eventSHA1    :: Encoded Hex    -- ^Current source code version at time of event
>                                  , event        :: ByteString     -- ^Payload
>                                  }
> 
> instance Show (EventType s) => Show (StoredEvent s) where
>   show (StoredEvent v t d s ev) = "StoredEvent " ++ show v ++ " " ++ show t ++ " " ++ show d ++ " " ++ show s ++ " " ++ show ev
> 
> instance Eq (EventType s) => Eq (StoredEvent s) where
>   (StoredEvent v t d s ev) == (StoredEvent v' t' d' s' ev') =  v == v' && t == t' && d == d' && s == s' && ev == ev'
> 
> instance ToJSON (EventType s) =>  ToJSON (StoredEvent s) where
>   toJSON (StoredEvent v t d s p) = object [
>     "eventVersion"    .= v,
>     "eventType"       .= t,
>     "eventDate"       .= toNanoSecs d,
>     "eventSHA1"       .= s,
>     "event"           .= A.String  (L.toStrict $ encodedText $ toBase64Text p)
>     ]
> 
> instance FromJSON (EventType s) => FromJSON (StoredEvent s) where
>   parseJSON (Object o) = StoredEvent
>                              <$> o .: "eventVersion"
>                              <*> o .: "eventType"
>                              <*> (fromNanoSecs <$> o .: "eventDate")
>                              <*> o .: "eventSHA1"
>                              <*> ((fromBase64Text . Encoded . L.fromStrict) <$>  o .: "event")
>   parseJSON _          = mempty
> 
> 
> -------------------------------------------------------------- Encoding ---------------------------------------------------------------------------
> data Base64
> data Hex
> 
> newtype Encoded code = Encoded { encodedText :: Text } deriving (Eq, Ord)
> 
> instance Show (Encoded s) where
>   show (Encoded t) = show t
> 
> instance Read (Encoded s) where
>   readsPrec n = List.map ( \ (t,s) -> (Encoded t, s)) . readsPrec n
> 
> instance ToJSON (Encoded Hex) where
>   toJSON (Encoded t) = String $ L.toStrict t
> 
> instance FromJSON (Encoded Hex) where
>   parseJSON (String t) = if T.all isHexDigit t
>                          then return $ Encoded (L.fromStrict t)
>                          else fail $ "not a valid hexadecimal encoded string: " ++ show t
>   parseJSON v          = fail $ "not a valid hexadecimal encoded string: "  ++ show v
> 
> instance IsString (Encoded Hex) where
>   -- not quite correct
>   fromString = Encoded . L.pack
> 
> toBase64Text :: ByteString -> Encoded Base64
> toBase64Text = Encoded . LE.decodeUtf8 . B64.encode
> 
> fromBase64Text :: Encoded Base64 -> ByteString
> fromBase64Text = B64.decodeLenient . LE.encodeUtf8 . encodedText
> 
> encodeBase64 :: ByteString -> ByteString
> encodeBase64 = B64.encode
> 
> currentSha1 :: Encoded Hex
> currentSha1 = "000000000000000000"
