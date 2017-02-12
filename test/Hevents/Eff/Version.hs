{-# LANGUAGE GADTs, AllowAmbiguousTypes, DataKinds, PolyKinds #-}
import           Data.ByteString    (ByteString)
import           Data.Proxy
import           Data.Serialize
import           Data.Text          (pack,Text)
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
  read  :: Proxy v -> ByteString -> Either String a
  write :: Proxy v -> a          -> ByteString

data Reader (v :: Nat) a where
  ReadVersion :: (ByteString -> Versioned v a) -> Reader v a
  
          
-- | Current version is a "global" constraint for some type
type family CurrentVersion a :: Nat
type instance CurrentVersion Obj3 = 3

load :: (Versionable (CurrentVersion a) a) => ByteString -> Either String [a]
load _ = undefined

-- A Versioned deserializer for type `a`
-- allows deserializing instances of `a` from version `v`, reifies applicative structure
data Versioned (v :: Nat) a where
  (:$:) ::             (a -> b) -> Versioned v a -> Versioned v b
  (:*:) :: Versioned v (a -> b) -> Versioned v a -> Versioned v b
  Atom  :: Get a                                 -> Versioned v a
  Cast  :: Versioned v' a                        -> Versioned v a
  -- ^Used for asserting that version `v` can be converted to version `v'`
  
infixl 4 :$:
infixl 4 :*:

doGet :: Versioned v a -> Get a
doGet (Atom g)   = g
doGet (Cast v)    = doGet v
doGet (f :$: v)   = f <$> doGet v
doGet (f :*: v)   = doGet f <*> doGet v

-- | Transforms a `Versioned v a` tree at some position
atIndex :: Int -> (a -> b) -> Versioned v a -> Versioned v' b
atIndex 0 f (Atom g)  = Atom (f <$> g)
atIndex 0 f (g :*: v) = Cast (atIndex 0 (f.) g :*: v)
atIndex 0 f (g :$: v) = Cast $ (f . g) :$: v 
atIndex n _         _ = error $ "Cannot transform Atom at index " ++ show n ++ " greater than 0"

class Migrator (v :: Nat) (v' :: Nat) a m where
  migrate :: (Versionable v a, Versionable v' a) => m v v' a -> Versioned v' a
    
data Migrate (v :: Nat) (v' :: Nat) a where
  Migrate :: Versioned v b -> Versioned v' (b -> a) -> Migrate v v' a

instance Migrator v v' a Migrate where
  migrate (Migrate v f) = f :*: Cast v

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

data Obj3 = Obj3 { f31 :: F1, f32 :: F2 }
  deriving (Show)

getint :: Versioned v Int
getint = Atom (get :: Get Int)

gettext :: Versioned v Text
gettext = Atom (get :: Get Text)

geto1 :: (Int -> Text -> Obj3) -> Versioned 1 Obj3
geto1 f = f :$: getint :*: Atom get

fromInt :: Int -> F1
fromInt = flip F1 ""

vf1 :: Migrate 1 2 F1
vf1 = Migrate getint (Atom (pure fromInt))

fromText :: Text -> F2
fromText = F2

vf2 :: Migrate 2 3 F2
vf2 = Migrate gettext (Atom (pure fromText))

geto2 :: (F1 -> Text -> Obj3) -> Versioned 2 Obj3
geto2 f = Cast $ geto1 (f.fromInt)

geto3 :: (F1 -> F2 -> Obj3) -> Versioned 3 Obj3
geto3 k = k :$: Atom get :*: Cast (migrate vf2)

instance Versionable 1 Obj1 where
  read    = undefined
  write _ = runPut . put
  
instance Versionable 2 Obj2 where
  read    = undefined
  write _ = runPut . put

instance Versionable 1 Obj3 where
   read _  = runGet $ doGet (geto1 (\ i t -> Obj3 (fromInt i) (fromText t)))
   write _ = undefined

instance Versionable 2 Obj3 where
   read _  = runGet $ doGet (geto2 $ \ i t -> Obj3 (fromInt i) (fromText t))
   write _ = undefined
   
-- a bytestring for version 
bs1 :: ByteString
bs1 = w (Obj1 12 "foo")
  where
    w = write (Proxy :: Proxy 1)

bs2 :: ByteString
bs2 = w (Obj2 (F1 12 "bar") "foo")
  where
    w = write (Proxy :: Proxy 2)
