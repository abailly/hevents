{-# LANGUAGE ImplicitParams #-}
{-| Expose file operations as a `Storage` effect -}
module Hevents.Eff.Store.FileStorage
  (module F) where

import           Control.Eff.Lift
import           Data.Either
import           Data.Monoid               ((<>))
import           Data.Text                 (pack)
import           Hevents.Eff.Store
import           Hevents.Eff.Store.FileOps as F
import           Prelude                   hiding (length, read)


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



