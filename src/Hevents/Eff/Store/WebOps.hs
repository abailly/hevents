{-| An instance of store exposed as a REST API.

This module defines only the API for this store and a client that can be passed to `Persist` command handler, but not the server.
-}
module Hevents.Eff.Store.WebOps(StoreAPI, storeAPI,
                                WebStorage, openWebStorage) where

import           Control.Monad.Except
import           Data.Aeson
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Base64   as Base64
import           Data.Either
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text                as Text
import           Data.Text.Encoding
import qualified Data.Text.Read           as Text
import qualified Data.Vector              as A
import           Hevents.Eff.Store
import           Hevents.Eff.Store.Events as Events
import           Network.HTTP.Client      (Manager, defaultManagerSettings, newManager)
import           Network.URI
import           Servant
import           Servant.Client
import           System.Clock

-- * API

-- | The Store is expected to expose only 2 operations: Put a new object, appending it to the
-- underlying log, and get all the logs. In order to accomodate for multiple clients using
-- a single endpoint, the API parameterizes each of these operations by a name
type StoreAPI =
  Capture "storeId" StoreId :> Capture "version" Version        :> Put    '[JSON] ()      :<|>
  -- Open given store at given version number
  Capture "storeId" StoreId                                     :> Delete '[JSON] ()      :<|>
  "store" :> Capture "storeId" StoreId :> ReqBody '[JSON] Event :> Put    '[JSON] ()      :<|>
  "store" :> Capture "storeId" StoreId                          :> Get    '[JSON] [Event] :<|>
  "store" :> Capture "storeId" StoreId                          :> Delete '[JSON] ()


storeAPI :: Proxy StoreAPI
storeAPI = Proxy

-- | An event is an opaque object containing some binary payload that will be decoded according
-- to the store type expected by the client
type Event = StoredEvent BS.ByteString

instance ToJSON Version where
  toJSON (Version v) = toJSON v

instance FromJSON Version where
  parseJSON (Number n) = return $ Version $ truncate n
  parseJSON e          = fail $ "cannot parse Version from JSON " ++ show e

instance FromHttpApiData Version where
  parseQueryParam t =
    case Text.decimal t of
      Right (n, _) -> Right $ Version n
      Left _       -> Left $ "cannot parse version number from " <> t

instance ToHttpApiData Version where
  toQueryParam (Version v) = Text.pack $ show v

instance ToJSON TimeSpec where
  toJSON (TimeSpec s ns) = toJSON $ [ toJSON s, toJSON ns ]

instance FromJSON TimeSpec where
  parseJSON (Array a) = TimeSpec             <$>
                        parseJSON (a A.! 0)  <*>
                        parseJSON (a A.! 1)
  parseJSON e         = fail $ "cannot parse Version from JSON " ++ show e


instance ToJSON SHA1 where
  toJSON (SHA1 sha) = toJSON sha

instance FromJSON SHA1 where
  parseJSON s = SHA1 <$> parseJSON s

instance ToJSON BS.ByteString where
  toJSON = String . decodeUtf8 . Base64.encode

instance FromJSON BS.ByteString where
  parseJSON (String s) = either
                         (fail . ("cannot decode ByteString from JSON string: " ++) . show)
                         return
                         (Base64.decode $ encodeUtf8 s)
  parseJSON e          = fail $ "cannot decode ByteString from JSON " ++ show e

instance ToJSON Event where
  toJSON StoredEvent{..} = object [ "version" .= eventVersion
                                  , "date"    .= eventDate
                                  , "sha1"    .= eventSHA1
                                  , "event"   .= event
                                  ]

instance FromJSON Event where
  parseJSON (Object o) = StoredEvent    <$>
                         o .: "version" <*>
                         o .: "date"    <*>
                         o .: "sha1"    <*>
                         o .: "event"
  parseJSON e          = fail $ "cannot decode StoredEvent from JSON " ++ show e

-- | Store name is simply a "string"
type StoreId = Text.Text

-- * Store Client Interface

type Port = Int

type WebM a = Manager -> BaseUrl -> ClientM a

openStore     :: StoreId  -> Version -> WebM ()
closeStore    :: StoreId             -> WebM ()
storeObject   :: StoreId  -> Event   -> WebM ()
loadObjects   :: StoreId             -> WebM [ Event ]
deleteObjects :: StoreId             -> WebM ()
(openStore :<|> closeStore :<|> storeObject :<|> loadObjects :<|> deleteObjects) = client storeAPI

data WebStorage = WebStorage { serverBaseURI   :: BaseUrl
                             , serverManager   :: Manager
                             , serverStoreName :: Text.Text
                             , serverClock     :: Clock
                             , serverVersion   :: Version
                             , serverSHA1      :: SHA1
                             }

instance Store IO WebStorage where
  close = closeStorage
  store = writeStorage
  load  = loadStorage
  reset = resetStorage

toString :: URI -> String
toString uri = uriToString id uri ""

openWebStorage :: Version -> SHA1 -> URI -> Scheme -> Port -> Text.Text -> IO WebStorage
openWebStorage version sha1 uri schem port name = do
  mgr <- newManager defaultManagerSettings
  let baseUrl = BaseUrl schem (toString uri) port ""
      storage = WebStorage baseUrl mgr name Realtime version sha1
  res <- runExceptT $ openStore name version mgr baseUrl
  case res of
    Left servError -> fail $ show servError
    Right ()       -> return storage

closeStorage :: WebStorage -> IO WebStorage
closeStorage ws@WebStorage{..} = do
  res <- runExceptT $ closeStore serverStoreName serverManager serverBaseURI
  case res of
    Left servError -> fail $ "cannot close remote storage: " <> show servError
    Right ()       -> return ws


writeStorage :: (Versionable event)
             => WebStorage
             -> IO (Either error event)
             -> (Either error (StorageResult event) -> IO result)
             -> IO (StorageResult result)
writeStorage WebStorage{..} pre post = do
  event <- pre
  case event of
    Right e    -> do
      timestamp <- getTime serverClock
      let storedEvent = StoredEvent serverVersion timestamp serverSHA1 $ write serverVersion e
      res <- runExceptT $ storeObject serverStoreName storedEvent serverManager serverBaseURI
      let finalResult = case res of
            Left servError -> OpFailed $ show servError
            Right ()       -> WriteSucceed e
      WriteSucceed <$> (post $ Right finalResult)
    Left err   -> WriteFailed <$> post (Left err)

loadStorage ::(Versionable event) => WebStorage -> IO (StorageResult event)
loadStorage WebStorage{..} = do
  res <- runExceptT $ loadObjects serverStoreName serverManager serverBaseURI
  case res of
    Left err  -> return $ OpFailed $ show err
    Right evs -> let (errs, events) = partitionEithers $ map (Events.read serverVersion . event) evs
                 in if null errs
                    then return $ LoadSucceed events
                    else return $ OpFailed (concat errs)



resetStorage ::WebStorage -> IO (StorageResult ())
resetStorage WebStorage{..} = do
  res <- runExceptT $ deleteObjects serverStoreName serverManager serverBaseURI
  case res of
    Left err -> return $ OpFailed $ show err
    Right _  -> return $ ResetSucceed


