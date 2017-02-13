{-# LANGUAGE AllowAmbiguousTypes, DataKinds, GADTs, PolyKinds #-}
import           Data.ByteString    (ByteString)
import           Data.Proxy
import           Data.Serialize
import           Data.Text          (Text, pack)
import           Data.Text.Encoding
import           GHC.TypeLits
import           Test.Hspec
import           Test.QuickCheck

-- Basic utility for serializing text as Utf8 encoded bytestring
instance Serialize Text where
  get = decodeUtf8 <$> get
  put = put . encodeUtf8

-- | A class instantiating a serializer/deserializer for some version
class Versionable (v :: Nat) a where
  reader :: Proxy v -> Get a
  writer :: Proxy v -> a -> Put

-- | A structure representing some type `a` as serialized to a `ByteString`
-- This structure ultimately holds a tree of `Get x`
data Versioned a where
  (:$:) ::     (a -> b) -> Versioned  a -> Versioned  b
  (:*:) :: Versioned  (a -> b) -> Versioned  a -> Versioned  b
  Atom  :: Get a                 -> Versioned  a
  Cast  :: Versioned a               -> Versioned  a
  
-- | Current version is a "global" constraint for some type
type family CurrentVersion :: Nat
type instance CurrentVersion = 3

class VersionUpTo (v :: Nat) a

instance (Versionable 1 a) => VersionUpTo 1 a

instance (Versionable v  a, VersionUpTo (v - 1) a) => VersionUpTo v a

load :: (VersionUpTo CurrentVersion a) => ByteString -> Either String [a]
load = runGet loadGetter
  where
    loadGetter = sequence $ repeat $ do
      v <- getInt32be
      case v of
        1 -> reader (Proxy :: Proxy 1)
        2 -> reader (Proxy :: Proxy 2)
        3 -> reader (Proxy :: Proxy 3)
        n -> fail $ "unknown version " ++ show n

  
infixl 4 :$:
infixl 4 :*:

doGet :: Versioned  a -> Get a
doGet (Atom g)   = g
doGet (f :$: v)   = f <$> doGet v
doGet (f :*: v)   = doGet f <*> doGet v

-- | Transforms a `Versioned  a` tree at some position
atIndex :: Int -> (a -> b) -> Versioned  a -> Versioned  b
atIndex 0 f (Atom g)  = Atom (f <$> g)
atIndex 0 f (g :*: v) = Cast (atIndex 0 (f.) g :*: v)
atIndex 0 f (g :$: v) = Cast $ (f . g) :$: v 
atIndex n _         _ = error $ "Cannot transform Atom at index " ++ show n ++ " greater than 0"

-- 3 different versions of the "same" structure

-- Version 1
data Obj1 = Obj1 { f11 :: Int, f12 :: Text }
  deriving (Show)

instance Serialize Obj1 where
  get = Obj1 <$> get <*> get
  put Obj1{..} = sequence_ [ put f11, put f12 ]

-- version 2 transforms a primitive field into a data type
data F1 = F1 { ff1 :: Int, ff2 :: Text }
  deriving (Show)

instance Serialize F1 where
   get = F1 <$> get <*> get
   put F1{..} = sequence_ [ put ff1, put ff2 ]

data Obj2 = Obj2 { f21 :: F1, f22 :: Text }
  deriving (Show)

instance Serialize Obj2 where
  get = Obj2 <$> get <*> get
  put Obj2{..} = sequence_ [ put f21, put f22 ]
    
-- version 3 transforms the nested data type
data F2 = F2 { ff12 :: Text }
  deriving (Show)

instance Serialize F2 where
  get = F2 <$> get
  put F2{..} = put ff12
  
data Obj3 = Obj3 { f31 :: F1, f32 :: F2 }
  deriving (Show)

getint :: Versioned  Int
getint = Atom (get :: Get Int)

gettext :: Versioned  Text
gettext = Atom (get :: Get Text)

geto1 :: (Int -> Text -> Obj3) -> Versioned Obj3
geto1 f = f :$: getint :*: Atom get

fromInt :: Int -> F1
fromInt = flip F1 ""

fromText :: Text -> F2
fromText = F2

getf2 :: (Text -> F2) -> Versioned F2
getf2 f = f :$: Atom get 
  
geto2 :: (F1 -> Text -> Obj3) -> Versioned Obj3
geto2 f = Cast $ geto1 (f.fromInt)

instance Versionable 1 Obj3 where
  reader _ = doGet $ Obj3 :$: (fromInt :$: getint) :*: (fromText :$: Atom get)
  writer _ = writer (Proxy :: Proxy 3)

instance Versionable 2 Obj3 where
  reader _ = doGet $ Obj3 :$: Atom get :*: (fromText :$: Atom get)
  writer _ = writer (Proxy :: Proxy 3)
   
instance Versionable 3 Obj3 where
   reader _          = doGet $ Obj3 :$: Atom get :*: getf2 F2
   writer _ Obj3{..} = put f31 >> put f32
   
-- a bytestring for version 
bs1 :: ByteString
bs1 = runPut $ put (Obj1 12 "foo")

bs2 :: ByteString
bs2 = runPut $ put (Obj2 (F1 12 "bar") "foo")

