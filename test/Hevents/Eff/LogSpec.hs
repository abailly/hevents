{-# LANGUAGE ScopedTypeVariables #-}
module Hevents.Eff.LogSpec(spec) where

import           Control.Exception
import           Data.Proxy
import           Hevents.Eff             as S
import           Prelude                 hiding (init)
import           System.Directory        (removeFile)
import           System.IO               (openTempFile)
import           Test.Hspec
import           Test.QuickCheck         as Q
import           Test.QuickCheck.Monadic as Q

prop_collectLogInAList :: [ Int ] -> Bool
prop_collectLogInAList list = let logged = S.run $ runLogPure $ mapM logE list
                              in  snd logged  == list

instance ToLogStr Int where
  toLogStr = toLogStr . show

prop_collectLogInAFile :: [ Int ] -> Property
prop_collectLogInAFile list = monadicIO $ do
  content <- Q.run $ bracket (openTempFile "." "test-log.tmp") (removeFile . fst) $ \ (f,_) -> do
    runLift $ runLogFile f (Proxy :: Proxy Int) $ mapM_ logE list
    readFile f

  Q.run $ putStrLn content

  Q.assert $ Prelude.read content == list

spec :: Spec
spec = describe "Log Effect" $ do

  it "collect logged items in a list" $ property $ prop_collectLogInAList
  it "collect logged items in a file" $ property $ prop_collectLogInAFile


