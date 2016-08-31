{-# LANGUAGE ImplicitParams         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilyDependencies #-}
-- | An effect which combines a `State` and a `Storage` within IO monad
module Hevents.Eff.Persist(Persist) where

import           Control.Concurrent.STM
import           Control.Eff.Exception
import           Control.Eff.Lift
import           Control.Exception
import           Data.ByteString
import           Data.IORef
import           Data.Monoid
import           Data.Text                  (pack)
import           Hevents.Eff.Model
import           Hevents.Eff.State
import           Hevents.Eff.State.InMemory (actAndApply)
import           Hevents.Eff.Store
import           Hevents.Eff.Store.FileOps
import           Prelude                    hiding (length)
import           System.IO
import           System.IO.Error

data Persist m = Persist { state :: IORef m
                           -- ^The persistent state managed by this effect
                         , store :: FileStorage
                         }

instance (Model m, Versionable (Event m)) => Registrar IO m (Persist m) where
  update p@Persist{..} (ApplyCommand c k) = lift (_runCommand p c) >>= k
  update Persist{..} (GetState k)         = lift (readIORef state) >>= k


_runCommand :: (Model m, Versionable (Event m)) => Persist m -> Command m -> IO (Either (Error m) (Event m))
_runCommand Persist{..} c = writeStoreCustom (_handler c) store >>= handleResult
  where
--    _handler :: (?currentVersion :: Version, Model m, Versionable (Event m)) => Command m -> OperationHandler (Event m)
    _handler c OpCustom (Just h) = do
      let ?currentVersion = storeVersion store
      st <- readIORef state
      let ev = st `act` c
      case ev of
        OK e -> do
          let s = doStore e
          opres <- (hSeek h SeekFromEnd 0 >> hPut h s >> hFlush h >> return (WriteSucceed e $ fromIntegral $ length s))
                   `catch` \ (ex  :: IOException) -> return (OpFailed $ "exception " <> show ex <> " while storing event")
          case opres of
            WriteSucceed _ _ -> modifyIORef state (`apply` e) >> return opres
            r                -> return r
        KO er -> return $ OpFailed $ "command resulted in error " --


    handleResult (WriteSucceed e _) = return $ Right e
    -- we throw IO exception which means things will blow up...
    -- need to find a way to transform arbitrary storage failures into Error m, e.g. by building
    -- Persist with appropriate function
    handleResult (OpFailed s)       = throwIO $ userError $ s
    handleResult _                  = throwIO $ userError $ "Unexpected result for store operation"
--  commandResult <- (`act` c) <$> readIORef state

applyEvent :: (Model m) => StorageResult  (Event m) -> Persist m -> IO ()
applyEvent (WriteSucceed s _) Persist{..} = modifyIORef state (`apply` s)
applyEvent _                  _           = return ()
