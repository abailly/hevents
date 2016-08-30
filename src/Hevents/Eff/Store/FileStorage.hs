{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hevents.Eff.Store.FileStorage where

import           Control.Concurrent       (myThreadId, threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Eff.Lift
import           Control.Exception        (Exception, IOException, catch, throw)
import           Control.Exception.Base   (BlockedIndefinitelyOnSTM)
import           Control.Monad            (forever)
import           Control.Monad.Trans      (liftIO)
import qualified Data.Binary.Get          as Bin
import           Data.ByteString          (ByteString, hGet, hPut, length)
import           Data.ByteString.Lazy     (fromStrict)
import           Data.Either
import           Data.Functor             (void)
import           Data.Monoid              ((<>))
import           Data.Serialize
import           Data.Text                (pack)
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

-- |Result of storage operations.
data StorageResult s where
  OpFailed :: { failureReason :: String } -> StorageResult s
  WriteSucceed :: Int -> StorageResult s
  LoadSucceed :: (Versionable s) => [s] -> StorageResult s
  ResetSucceed :: StorageResult s
  NoOp :: StorageResult s

data StoreOperation s where
  OpStore :: Versionable s => s -> TMVar (StorageResult s) -> StoreOperation s
  OpLoad  :: Versionable s => TMVar (StorageResult s) -> StoreOperation s
  OpReset :: TMVar (StorageResult s) -> StoreOperation s

data QueuedOperation where
  QueuedOperation :: forall s . Versionable s => StoreOperation s -> QueuedOperation

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

runStorage :: FileStorage -> IO ()
runStorage FileStorage{..} = do
  threadDelay 1000000
  forever $ do
    QueuedOperation op <- (atomically $ readTBQueue storeTQueue) `catch`
      (\ (e :: BlockedIndefinitelyOnSTM) -> myThreadId >>= (\ i -> putStrLn ("got blocked while dequeuing in thread " <> show i)) >> throw e)
    let ?currentVersion  = storeVersion
    void $ runOp op storeHandle

runOp :: (?currentVersion :: Version, Versionable s) => StoreOperation s -> Maybe Handle -> IO (StorageResult s)
runOp _ Nothing = return $ NoOp
runOp (OpStore e r) (Just h) =
  do
    let s = doStore e
    opres <- (hSeek h SeekFromEnd 0 >> hPut h s >> hFlush h >> return (WriteSucceed $ fromIntegral $ length s))
             `catch` \ (ex  :: IOException) -> return (OpFailed $ "exception " <> show ex <> " while storing event")
    atomically $ putTMVar r opres
    return opres

runOp (OpLoad r) (Just h)  =  do
  pos <- hTell h
  hSeek h SeekFromEnd 0
  sz <- hTell h
  hSeek h AbsoluteSeek 0
  opres <- readAll h sz
  hSeek h AbsoluteSeek pos
  atomically $ putTMVar r (LoadSucceed opres)
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
runOp (OpReset r) (Just handle) =
  do
    w <- hIsWritable handle
    opres <- case w of
     False -> return $ OpFailed "File handle not writeable while resetting event store"
     True ->  do
       emptyEvents `catch` \ (ex  :: IOException) -> return (OpFailed $ "exception" <> (show ex) <> " while resetting event store")
         where emptyEvents = do
                 (hSetFileSize handle 0)
                 return ResetSucceed
    atomically $ putTMVar r $ opres
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

push :: (Versionable s) => (TMVar (StorageResult s) -> StoreOperation s) -> FileStorage ->  IO (StorageResult s)
push op FileStorage{..} = do
        v <- atomically $ do
          tmv <- newEmptyTMVar
          writeTBQueue storeTQueue (QueuedOperation $ op tmv)
          return tmv
        atomically $ takeTMVar v

writeStore :: (Versionable s) => s -> FileStorage -> IO (StorageResult s)
writeStore s = push (OpStore s)

readStore :: (Versionable s) => FileStorage -> IO (StorageResult s)
readStore = push OpLoad

resetStore :: FileStorage -> IO (StorageResult ())
resetStore = push OpReset


instance Storage IO FileStorage where
  persist storage (Store x k)    = lift (writeStore x storage) >>= k . handleStoreResult
    where
      handleStoreResult (WriteSucceed _) = Right x
      handleStoreResult (OpFailed s)     = Left $ IOError $ "Failed to properly store value: " <> pack s
      handleStoreResult _                = Left $ IOError $ "Unexpected result for store operation"

  persist storage (Load Offset{..} Count{..} _ k) = lift (readStore storage) >>= k . handleLoadResult
    where
      handleLoadResult (LoadSucceed xs) = Right $ take (fromIntegral count) $ drop (fromIntegral offset) $ xs
      handleLoadResult (OpFailed s)     = Left $ IOError $ "Failed to properly load values: " <> pack s
      handleLoadResult _                = Left $ IOError $ "Unexpected result for store operation"

  persist storage (Reset k)      = lift (resetStore storage) >>= k . handleResetResult
    where
      handleResetResult ResetSucceed = Right ()
      handleResetResult (OpFailed s) = Left $ IOError $ "Failed to properly reset store: " <> pack s
      handleResetResult _            = Left $ IOError $ "Unexpected result for reset operation"



