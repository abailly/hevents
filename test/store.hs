{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad            (forever)
import           Hevents.Eff

import           System.IO
-- import           Test.QuickCheck
-- import           Test.QuickCheck.Monadic as Q

withStore :: (FileStorage -> IO a) -> IO a
withStore = bracket (openFileStorage "test.store") closeFileStorage

data Op = Op String (TMVar Int)

storerun :: TBQueue Op -> IO ()
storerun q = do
  h <- openFile "store.test" ReadWriteMode
  hSetBuffering h NoBuffering
  forever $ do
    Op s v <- atomically $ readTBQueue q
    hPutStrLn h s
    atomically $ putTMVar v (length s)


main :: IO ()
main = do
  q <- newTBQueueIO 100
  _ <- async $ storerun q
  storeInput q
  where
    storeInput q = forever $ do
      putStrLn "pushing"
      l <- getLine
      v <- newEmptyTMVarIO
      r <- atomically $ do
        writeTBQueue q (Op l v)
        takeTMVar v
      putStrLn $ "got " ++ show r


