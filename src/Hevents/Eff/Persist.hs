{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE ImplicitParams         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilyDependencies #-}
-- | An effect which combines a `State` and a `Storage` within IO monad
module Hevents.Eff.Persist(Persist) where

import           Control.Concurrent.STM
import           Control.Eff.Exception
import           Control.Eff.Lift
import           Control.Exception
import           Data.ByteString            hiding (pack)
import           Data.IORef
import           Data.Monoid
import           Data.Serialize
import           Data.Text                  (pack)
import           GHC.Generics
import           Hevents.Eff.Model
import           Hevents.Eff.State
import           Hevents.Eff.State.InMemory (actAndApply)
import           Hevents.Eff.Store
import           Hevents.Eff.Store.FileOps
import           Prelude                    hiding (length)
import           System.IO
import           System.IO.Error

data Persist m = Persist { state        :: IORef m
                           -- ^The persistent state managed by this effect
                         , store        :: FileStorage
                         , errorHandler :: StoreError -> Error m
                           -- ^How to interpret storage errors within the model
                         }

instance (Model m, Versionable (Error m), Versionable (Event m)) => Registrar IO m (Persist m) where
  update p@Persist{..} (ApplyCommand c k) = lift (runCommand p c) >>= k
  update Persist{..} (GetState k)         = lift (readIORef state) >>= k

data CommandResult m = Success (Event m)
                     | Failure (Error m)
                     | Fatal StoreError
                       deriving (Generic)

instance (Versionable (Event m), Serialize (Error m)) => Versionable (CommandResult m)
instance (Serialize (Event m), Serialize (Error m)) => Serialize (CommandResult m)

runCommand :: (Model m, Versionable (Event m), Versionable (CommandResult m)) => Persist m -> Command m -> IO (Either (Error m) (Event m))
runCommand Persist{..} c = writeStoreCustom (go c) store >>= return . handleResult
  where
    go c OpCustom (Just h) = flip WriteSucceed 0 <$> do
      let ?currentVersion = storeVersion store
      st <- readIORef state
      let ev = st `act` c
      case ev of
        OK e -> do
          let s = doStore e
          opres <- (hSeek h SeekFromEnd 0 >> hPut h s >> hFlush h >> return (WriteSucceed e $ fromIntegral $ length s))
                   `catch` \ (ex  :: IOException) -> return (OpFailed $ "exception " <> show ex <> " while storing event")
          case opres of
            WriteSucceed _ _ -> modifyIORef state (`apply` e) >> return (Success e)
            OpFailed s       -> return $ Fatal (IOError $ pack s)
            _                -> return $ Fatal (IOError "Something got wrong...")
        KO er -> return $ Failure er
    go _ _ _               = return $ OpFailed $ "should not happen: don't know how to interpret command and store operation in custom context"

    handleResult (WriteSucceed (Success e)  _) = Right e
    handleResult (WriteSucceed (Failure er) _) = Left er
    handleResult (WriteSucceed (Fatal er)   _) = Left $ errorHandler er
