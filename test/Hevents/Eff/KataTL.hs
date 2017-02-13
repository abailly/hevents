{-# LANGUAGE AllowAmbiguousTypes, DataKinds, PolyKinds #-}
import           Data.Proxy
import           Data.Text          (Text)
import           Data.Text.Encoding
import           GHC.TypeLits
import           Test.Hspec
import           Test.QuickCheck

-- 3 different versions of the "same" structure
-- We only care about

data Obj1 = Obj1 { f11 :: Int, f12 :: Text }

-- version 2 transforms a primitive field into a data type
data F1 = F1 { ff1 :: Int, ff2 :: Text }
data Obj2 = Obj2 { f21 :: F1, f22 :: Text }

-- version 3 transforms the nested data type
data F2 = F2 { ff12 :: Text }
data Obj3 = Obj3 { f31 :: F2, f32 :: Text }

-- final version, same structure than version 3
data F = F { f' :: Text }
data Obj = Obj { f :: F, g :: Text }


data Ver = Ver Nat

-- Migration type class
-- Parameterized with 2 versions and a functor
class Migrate (v :: Ver) (v' :: Ver) f where
  migrate :: f v a -> f v' a

