{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
import           Data.Proxy
import           Data.String
import           GHC.TypeLits
import           Test.Hspec
import           Test.QuickCheck

data Digit = I
           | IV
           | V
           | IX
           | X

type family FromRoman (r :: [Digit]) :: Nat where
  FromRoman '[]        = 0
  FromRoman ('I ': ds) = 1 + FromRoman ds
  FromRoman '[IV]      = 4
  FromRoman ('V ': ds) = 5 + FromRoman ds
  FromRoman '[IX]      = 9
  FromRoman ('X ': ds) = 10 + FromRoman ds

