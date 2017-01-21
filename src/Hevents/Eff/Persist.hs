{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE ImplicitParams         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilyDependencies #-}
-- | An effect which combines a `State` and a `Storage` within IO monad
module Hevents.Eff.Persist(Persist, makePersist, stopPersist,
                           state) where

import           Control.Eff.Lift
import           Data.IORef
import           Data.Serialize
import           Data.Text                 (pack)
import           GHC.Generics
import           Hevents.Eff.Model
import           Hevents.Eff.State
import           Hevents.Eff.Store         
import           Prelude                   hiding (length)

data Persist m s = Persist { state        :: IORef m
                             -- ^The persistent state managed by this effect
                           , storeEngine  :: s
                           -- ^The low-level storage engine
                           , errorHandler :: StoreError -> Error m
                             -- ^How to interpret storage errors within the model
                           }

makePersist :: m -> s -> (StoreError -> Error m) -> IO (Persist m s)
makePersist m s h = Persist <$> newIORef m <*> pure s <*> pure h

stopPersist :: (Store IO s) => Persist m s -> IO (Persist m s)
stopPersist p@Persist{..} = close storeEngine >> return p

instance (Model m, Store IO s, Versionable (Event m)) => Registrar IO m (Persist m s) where
  update p@Persist{..} (ApplyCommand c k) = lift (runCommand p c) >>= k
  update Persist{..} (GetState k)         = lift (readIORef state) >>= k

data CommandResult m = Success (Event m)
                     | Failure (Error m)
                     | Fatal StoreError
                       deriving (Generic)

instance (Show (Event m), Show (Error m)) => Show (CommandResult m) where
  show (Success e) = "[Success] " ++ show e
  show (Failure f) = "[Failure] " ++ show f
  show (Fatal   e) = "[Fatal  ] " ++ show e
   
instance (Versionable (Event m), Versionable (Error m)) => Versionable (CommandResult m)
instance (Serialize (Event m), Serialize (Error m)) => Serialize (CommandResult m)

runCommand :: (Model m, Store IO s, Versionable (Event m))
              => Persist m s -> Command m -> IO (Either (Error m) (Event m))
runCommand Persist{..} command = store storeEngine pre post  >>= return . handleResult
  where
    pre = do
      st <- readIORef state
      case st `act` command of
        OK e -> return $ Right e
        KO e -> return $ Left e

    post (Right (WriteSucceed ev )) = modifyIORef state (`apply` ev) >> return (Success ev)
    post (Right (OpFailed f))       = return $ Fatal (IOError $ pack f)
    post (Right _)                  = return $ Fatal (IOError "Something got wrong...")
    post (Left er)                  = return $ Failure er
    
    handleResult (WriteSucceed (Success e)) = Right e
    handleResult (WriteFailed (Failure er)) = Left er
    handleResult (WriteFailed (Fatal er))   = Left $ errorHandler er
    handleResult _                          = Left $ errorHandler (IOError $ pack $ "Something got wrong while trying to store result of command")
