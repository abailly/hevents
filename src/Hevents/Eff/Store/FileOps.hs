{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-| Low-level file storage engine -}
module Hevents.Eff.Store.FileOps where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception        (Exception, IOException, bracket, catch)
import           Control.Monad            (forever)
import           Control.Monad.Trans      (liftIO)
import qualified Data.Binary.Get          as Bin
import           Data.ByteString          (ByteString, hGet, hPut, length)
import           Data.ByteString.Lazy     (fromStrict)
import           Data.Either
import           Data.Functor             (void)
import           Data.Monoid              ((<>))
import           Data.Serialize
import           Data.Typeable
import           Hevents.Eff.Store
import           Prelude                  hiding (length, read)
import           System.IO


-- |Internal definition of the storage to use for operations
data FileStorage = FileStorage  { storeName    :: String
                                , storeVersion :: Version
                                , storeHandle  :: Maybe Handle
                                                 -- ^Handle to the underlying OS stream for storing events
                                , storeTid     :: TMVar (Async ())
                                                 -- ^The store's thread id, if store is open and running
                                , storeTQueue  :: TBQueue QueuedOperation
                                }

data QueuedOperation where
  QueuedOperation :: forall s . 
    { operation :: StoreOperation IO s,
      opResult :: TMVar (StorageResult s) } -> QueuedOperation

data StorageException = CannotDeserialize String
                      deriving (Show, Typeable)

instance Exception StorageException

openFileStorage :: FilePath -> IO FileStorage
openFileStorage file = do
  tidvar  <- atomically newEmptyTMVar
  tq      <- newTBQueueIO 100  -- TODO remove magic number
  h <- openFile file ReadWriteMode
  hSetBuffering h NoBuffering
  let s@FileStorage{..} = FileStorage file (Version 1) (Just h) tidvar tq
  tid <- async (runStorage s)
  atomically $ putTMVar storeTid tid
  return s

openHandleStorage :: Handle -> IO FileStorage
openHandleStorage hdl = do
  tidvar  <- atomically newEmptyTMVar
  tq      <- newTBQueueIO 100
  hSetBuffering hdl NoBuffering
  let s@FileStorage{..} = FileStorage "<handle>" (Version 1) (Just hdl) tidvar tq
  tid <- async (runStorage s)
  atomically $ putTMVar storeTid tid
  return s

closeFileStorage :: FileStorage -> IO FileStorage
closeFileStorage s@(FileStorage _ _ h ltid _) = do
  t <- liftIO $ atomically $ tryTakeTMVar ltid
  case t of
   Just tid -> liftIO $ cancel tid
   Nothing  -> return ()
  void $ hClose `traverse` h
  return s

-- | Run some computation requiring a `FileStorage`, automatically opening and closing required
-- file.
withStorage :: FilePath -> (FileStorage -> IO a) -> IO a
withStorage fp = bracket (openFileStorage fp) closeFileStorage

runStorage :: FileStorage -> IO ()
runStorage FileStorage{..} = do
  forever $ do
    QueuedOperation op res <- atomically $ readTBQueue storeTQueue
    let ?currentVersion  = storeVersion
    atomically . putTMVar res =<< runOp op storeHandle


runOp :: (?currentVersion::Version) =>
         StoreOperation IO s -> Maybe Handle -> IO (StorageResult s)
runOp _           Nothing         = return NoOp
runOp (OpStore pre post) (Just h) =
  do
    p <- pre
    case p of
      Right ev -> do
        let s = doStore ev
        opres <- (hSeek h SeekFromEnd 0 >> hPut h s >> hFlush h >> return (WriteSucceed ev))
                 `catch` \ (ex  :: IOException) -> return (OpFailed $ "exception " <> show ex <> " while storing event")
        WriteSucceed <$> (post $ Right opres)
      Left  l -> WriteFailed <$> post (Left l)

runOp OpLoad      (Just h) =  do
  pos <- hTell h
  hSeek h SeekFromEnd 0
  sz <- hTell h
  hSeek h AbsoluteSeek 0
  opres <- readAll h sz
  hSeek h AbsoluteSeek pos
  return $ LoadSucceed opres
    where
      readAll :: (?currentVersion :: Version, Versionable s) => Handle -> Integer -> IO [s]
      readAll hdl sz =  if sz > 0 then
                          do
                            (loaded,ln) <- doLoad hdl
                            case loaded of
                              Right e -> do
                                es     <- readAll hdl (sz - ln)
                                return $ e : es
                              Left err -> fail err
                        else
                          return []
runOp OpReset (Just handle) =
  do
    w <- hIsWritable handle
    opres <- case w of
     False -> return $ OpFailed "File handle not writeable while resetting event store"
     True ->  do
       emptyEvents `catch` \ (ex  :: IOException) -> return (OpFailed $ "exception" <> (show ex) <> " while resetting event store")
         where emptyEvents = do
                 (hSetFileSize handle 0)
                 return ResetSucceed
    return opres


-- | Convert a serializable to ByteString for binary storage
doStore :: (?currentVersion :: Version, Versionable s) => s -> ByteString
doStore e = let bs = write ?currentVersion e
                crc = 42  -- TODO compute real CRC32
            in runPut $ do
  putWord32be $ fromIntegral (length bs + 4 + 1)
  putWord8 (fromIntegral $ version ?currentVersion)
  putWord32be crc
  putByteString bs

-- |Read a single event from file store, returning also the number of bytes read
--
-- This is not symetric to doStore as we need first to read the length of the message, then
-- to read only the necessary amount of bytes from storage
doLoad :: Versionable s => Handle -> IO (Either String s, Integer)
doLoad  h = do
  lw <- hGet h 4
  let l = fromIntegral $ Bin.runGet Bin.getWord32be $ fromStrict lw
  bs <- hGet h l
  let msg = do
        v   <- getWord8
        _   <- getWord32be
        pay <- getByteString  (l - 5)
        either fail return $ read (fromIntegral v) pay
      content = runGet msg bs
  return $ (content, fromIntegral $ l + 4)

push :: StoreOperation IO s -> FileStorage ->  IO (StorageResult s)
push op FileStorage{..} = do
        v <- atomically $ do
          tmv <- newEmptyTMVar
          writeTBQueue storeTQueue (QueuedOperation op tmv)
          return tmv
        atomically $ takeTMVar v

writeStore :: (Versionable e) => FileStorage -> IO (Either a e) -> (Either a (StorageResult e) -> IO r) -> IO (StorageResult r)
writeStore s pre post = push (OpStore pre post) s

readStore :: (Versionable s) => FileStorage -> IO (StorageResult s)
readStore = push OpLoad 

resetStore :: FileStorage -> IO (StorageResult ())
resetStore = push OpReset


instance Store IO FileStorage where
  close = closeFileStorage
  store = writeStore
  load = readStore
  reset = resetStore
