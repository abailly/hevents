{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hevents.Eff.Store.FileStorage where

import           Control.Concurrent.Async
import           Control.Concurrent.STM   (TBQueue, TMVar, TVar, atomically,
                                           modifyTVar', newEmptyTMVar,
                                           newTBQueueIO, newTVar, putTMVar,
                                           readTBQueue, readTVar, readTVarIO,
                                           takeTMVar, tryReadTMVar,
                                           tryTakeTMVar, writeTBQueue)
import           Control.Concurrent.STM
import           Control.Eff.Lift
import           Control.Exception        (Exception, SomeException, catch)
import           Control.Monad            (forever)
import           Control.Monad.Trans      (liftIO)
import qualified Data.Binary.Get          as Bin
import           Data.ByteString          (ByteString, hGet, hPut, length)
import           Data.Either              (isRight)
import           Data.Either
import           Data.Functor             (void)
import qualified Data.Map                 as M
import           Data.Monoid              ((<>))
import           Data.Serialize
import           Data.Serialize.Get
import qualified Data.Set                 as S
import           Data.Text                (pack)
import qualified Data.Text                as T
import           Data.Typeable
import           Hevents.Eff.Store
import           Prelude                  hiding (length, read)
import           System.IO


-- |Internal definition of the storage to use for operations
data FileStorage = FileStorage  { storeName    :: String
                                , storeHandle  :: Maybe Handle
                                                   -- ^Handle to the underlying OS stream for storing events
                                , storeTid     :: TMVar (Async ())
                                                   -- ^The store's thread id, if store is open and running
                                 , storeTQueue :: TBQueue StoreOperation
                                }

-- |Result of storage operations.
data StorageResult s where
  OpFailed :: { failureReason :: String } -> StorageResult ()
  WriteSucceed :: Int -> StorageResult s
  LoadSucceed :: (Serializable s) => [s] -> StorageResult s
  ResetSucceed :: StorageResult ()
  NoOp :: StorageResult ()

data StoreOperation where
  OpStore :: Serializable s => s -> TMVar (StorageResult s) -> StoreOperation
  OpLoad  :: TMVar (StorageResult s) -> StoreOperation
  OpReset :: TMVar (StorageResult ()) -> StoreOperation

data StorageException = CannotDeserialize String
                      deriving (Show, Typeable)

instance Exception StorageException

openFileStorage :: FilePath -> IO FileStorage
openFileStorage file = do
  tidvar  <- atomically newEmptyTMVar
  tq      <- newTBQueueIO 100  -- TODO remove magic number
  h <- openFile file ReadWriteMode
  hSetBuffering h NoBuffering
  let s@FileStorage{..} = FileStorage file (Just h) tidvar tq
  tid <- async (runStorage s)
  atomically $ putTMVar storeTid tid
  return s

openHandleStorage :: Handle -> IO FileStorage
openHandleStorage hdl = do
  tidvar  <- atomically newEmptyTMVar
  tq      <- newTBQueueIO 100
  hSetBuffering hdl NoBuffering
  let s@FileStorage{..} = FileStorage "<handle>" (Just hdl) tidvar tq
  tid <- async (runStorage s)
  atomically $ putTMVar storeTid tid
  return s

closeFileStorage :: FileStorage -> IO FileStorage
closeFileStorage s@(FileStorage _ h ltid _) = do
  t <- liftIO $ atomically $ tryTakeTMVar ltid
  case t of
   Just tid -> liftIO $ cancel tid
   Nothing  -> return ()
  void $ hClose `traverse` h
  return s

runStorage :: FileStorage -> IO ()
runStorage FileStorage{..} = forever $ do
  op <- atomically $ readTBQueue storeTQueue
  void $ runOp op storeHandle

runOp :: (?currentVersion :: Version, Serializable s) => StoreOperation -> Maybe Handle -> IO (StorageResult s)
runOp _ Nothing = return $ NoOp
runOp (OpStore e r) (Just h) =
  do
    let s = doStore e
    opres <- (hSeek h SeekFromEnd 0 >> hPut h s >> hFlush h >> return (WriteSucceed $ fromIntegral $ length s))
             `catch` \ (ex  :: SomeException) -> return (OpFailed $ "exception " <> show ex <> " while storing event ")
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
      readAll :: (?currentVersion :: Version, Serializable s) => Handle -> Integer -> IO [s]
      readAll hdl sz =  if sz > 0 then
                          do
                            (e,ln) <- doLoad hdl
                            es     <- readAll hdl (sz - ln)
                            return $ e : es
                        else
                          return []
runOp (OpReset r) (Just handle) =
  do
    w <- (hIsWritable handle)
    opres <- case w of
     False -> return $ OpFailed "File handle not writeable while resetting event store"
     True ->  do
       -- TODO catching all exceptions not a good idea, http://hackage.haskell.org/package/base-4.7.0.2/docs/Control-Exception.html#g:4
       -- but can't find out what hsetFileSize throws.
       v <- emptyEvents `catch` \ (ex  :: SomeException) -> return (OpFailed $ "exception" <> (show ex) <> " while resetting event store")
       atomically $ putTMVar r v
       return v
       where emptyEvents = do
               (hSetFileSize handle 0)
               return ResetSucceed
    atomically $ putTMVar r $ opres
    return opres


-- | Convert a serializable to ByteString for binary storage
doStore :: (?currentVersion :: Version, Serializable s) => s -> ByteString
doStore e = let bs = write ?currentVersion e
                crc = 42  -- TODO compute real CRC32
            in runPut $ do
  putWord32be $ fromIntegral (length bs + 4 + 1)
  putWord8 (fromIntegral ?currentVersion)
  putWord32be crc
  putByteString bs

-- |Read a single event from file store, returning also the number of bytes read
--
-- This is not symetric to doStore as we need first to read the length of the message, then
-- to read only the necessary amount of bytes from storage
doLoad :: Serializable s => Handle -> IO (s, Integer)
doLoad  h = do
  lw <- hGet h 4
  let l = fromIntegral $ Bin.runGet Bin.getWord32be lw
  bs <- hGet h l
  let msg = do
        v   <- getWord8
        crc <- getWord32be
        pay <- getByteString  (l - 5)
        return $ read (fromIntegral v) pay
      Right content = runGet msg bs
  return $ (content, fromIntegral $ l + 4)

instance Storage STM FileStorage where
  persist FileStorage{..} (Store x k)    = lift (enqueueStore x) >>= k . handleStoreResult
    where
      enqueueStore x = do
        tmv <- newEmptyTMVar
        writeTBQueue storeTQueue (OpStore x tmv)
        readTMVar tmv

      handleStoreResult (WriteSucceed n) = Right x
      handleStoreResult (OpFailed s)     = Left $ IOError $ "Failed to properly store value: " <> pack s
      handleStoreResult r                = Left $ IOError $ "Unexpected result for store operation"

  persist FileStorage{..} (Load Offset{..} Count{..} g k) = lift enqueueLoad >>= k . handleLoadResult
    where
      enqueueLoad = do
        tmv <- newEmptyTMVar
        writeTBQueue storeTQueue (OpLoad tmv)
        readTMVar tmv

      handleLoadResult (LoadSucceed xs) = Right $ map g xs
      handleLoadResult (OpFailed s)     = Left $ IOError $ "Failed to properly load values: " <> pack s
      handleLoadResult r                = Left $ IOError $ "Unexpected result for store operation"

      checkErrors xs = case partitionEithers xs of
        ([],rs)   -> Right $ reverse $ take (fromIntegral count) $ drop (fromIntegral offset) $ rs
        ((e:_),_) -> Left  $ IOError $ pack e
  persist FileStorage{..} (Reset k)      = lift enqueueReset >>= k . handleResetResult
    where
      enqueueReset = do
        tmv <- newEmptyTMVar
        writeTBQueue storeTQueue (OpReset tmv)
        readTMVar tmv

      handleResetResult ResetSucceed = Right ()
      handleResetResult (OpFailed s) = Left $ IOError $ "Failed to properly reset store: " <> pack s
      handleResetResult r            = Left $ IOError $ "Unexpected result for reset operation"


