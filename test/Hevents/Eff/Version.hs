{-# LANGUAGE PatternSynonyms, AllowAmbiguousTypes, DataKinds, GADTs, PolyKinds #-}
import           Data.ByteString    (ByteString)
import           Data.Proxy
import           Data.Serialize
import           Data.Text          (Text, pack)
import           Data.Text.Encoding
import           GHC.TypeLits as Types
import           Test.Hspec
import           Test.QuickCheck

-- | "Horizontal" composition, to enumerate arguments
type a :-: b = (a, b)
infixr 5 :-:

pattern (:-:) :: a -> b -> (a ,b)
pattern a :-: b = (a ,b)

-- | Vertical composition. "applies" A function to its arguments, yielding a result
data Ap f a b where
  Ap :: (Apply f a b) => f -> a -> Ap f a b

class Apply f a b | f a -> b where
  apply :: f -> a -> b

instance Apply (a -> b) a b where
  apply f a = f a

-- instance Apply (a -> b) (a :&: ()) b where
--   apply f (a :&: _) = f a

instance (Apply (b -> c) b c) => Apply (a -> b -> c) (a :-: b) c where
  apply f (a :-: b) = apply (f a) b

instance Apply (a -> b) (Ap f c a) b where
  apply f ap = f $ apply ap ()
  
instance (Apply (b -> c) b c) =>  Apply (a -> b -> c) (Ap f c a :-: b) c where
  apply f (ap :-: b) = apply (f $ apply ap ()) b
  
instance Apply (Ap f a b) d b where
  apply (Ap f a) _ = apply f a
  
  
-- | Index over a tree of applications and arguments
-- Need a closed type family otherwise overlapping instances conflict
type family (:!) a (p :: [Nat]) :: * where
  a                  :! '[]     = a
  (Ap f a b)         :! (0 : k) = a :! k
  (a :-: b)          :! (0 : k) = a :! k
  (a :-: b)          :! (n : k) = b :! (n -1 : k)
  a                  :! '[0]    = a
  a                  :! k       = TypeError (Types.Text "The type " :<>: ShowType a :<>:
                                             Types.Text " is not indexable with " :<>: ShowType k )


                                   
-- -- Basic utility for serializing text as Utf8 encoded bytestring
-- instance Serialize Text where
--   get = decodeUtf8 <$> get
--   put = put . encodeUtf8

-- -- | A class instantiating a serializer/deserializer for some version
-- class Versionable (v :: Nat) a where
--   reader :: Proxy v -> Get a
--   writer :: Proxy v -> a -> Put

-- type f :#: b = (f,b)

-- pattern f :#: b = (f,b)

-- -- | A structure representing some type `a` as serialized to a `ByteString`
-- -- This structure ultimately holds a tree of `Get x`
-- data Versioned a where
--   (:$:) :: (Apply f a b) =>        f -> Versioned  a -> Versioned  (f :#: a)
--   Resolve :: (Apply f a b) => Versioned (f :#: a) -> Versioned b
-- --  (:*:) :: (Apply f a b) => Versioned f -> Versioned  a -> Versioned  b
--   (:|:) ::                  Versioned a -> Versioned  b -> Versioned  (a :&: b)
--   Atom  ::                        Get a                 -> Versioned  a
  
-- -- -- | Current version is a "global" constraint for some type
-- -- type family CurrentVersion :: Nat
-- -- type instance CurrentVersion = 3

-- -- class VersionUpTo (v :: Nat) a

-- -- instance (Versionable 1 a) => VersionUpTo 1 a

-- -- instance (Versionable v  a, VersionUpTo (v - 1) a) => VersionUpTo v a

-- -- load :: (VersionUpTo CurrentVersion a) => ByteString -> Either String [a]
-- -- load = runGet loadGetter
-- --   where
-- --     loadGetter = sequence $ repeat $ do
-- --       v <- getInt32be
-- --       case v of
-- --         1 -> reader (Proxy :: Proxy 1)
-- --         2 -> reader (Proxy :: Proxy 2)
-- --         3 -> reader (Proxy :: Proxy 3)
-- --         n -> fail $ "unknown version " ++ show n

  
-- infixl 4 :$:
-- infixr 5 :|:

-- -- doGet :: Versioned  a -> Get a
-- -- doGet (Atom g)   = g
-- -- doGet (f :$: v)   = apply f <$> doGet v
-- -- doGet (a :|: b)   = apply doGet f <*> doGet v

-- -- -- | Transforms a `Versioned  a` tree at some position
-- -- atIndex :: (Apply f a b) => Int -> Versioned a -> Versioned  b -> Versioned b
-- -- atIndex 0 f (Atom g)  = Atom (apply f <$> g)
-- -- atIndex 0 f (a :|: b) = Atom (apply f <$> a)
-- -- atIndex n _         _ = error $ "Cannot transform Atom at index " ++ show n ++ " greater than 0"

-- -- 3 different versions of the "same" structure

-- -- Version 1
-- data Obj1 = Obj1 { f11 :: Int, f12 :: Text }
--   deriving (Show)

-- instance Serialize Obj1 where
--   get = Obj1 <$> get <*> get
--   put Obj1{..} = sequence_ [ put f11, put f12 ]

-- version 2 transforms a primitive field into a data type
data F1 = F1 { ff1 :: Int, ff2 :: Text }
  deriving (Show)

-- instance Serialize F1 where
--    get = F1 <$> get <*> get
--    put F1{..} = sequence_ [ put ff1, put ff2 ]

-- data Obj2 = Obj2 { f21 :: F1, f22 :: Text }
--   deriving (Show)

-- instance Serialize Obj2 where
--   get = Obj2 <$> get <*> get
--   put Obj2{..} = sequence_ [ put f21, put f22 ]
    
-- version 3 transforms the nested data type
data F2 = F2 { ff12 :: Text }
  deriving (Show)

-- instance Serialize F2 where
--   get = F2 <$> get
--   put F2{..} = put ff12
  
data Obj3 = Obj3 { f31 :: F1, f32 :: F2 }
  deriving (Show)

data Obj4 = Obj4 { f4 :: F1 }
  deriving (Show)

-- getint :: Versioned  Int
-- getint = Atom (get :: Get Int)

-- gettext :: Versioned  Text
-- gettext = Atom (get :: Get Text)

-- fromInt :: Int -> F1
-- fromInt = flip F1 ""

-- fromText :: Text -> F2
-- fromText = F2

-- getf2 :: (Text -> F2) -> Versioned F2
-- getf2 f = Resolve $ f :$: gettext 

-- doGet :: Versioned a -> Get a
-- doGet (f :$: a) = apply f <$> doGet a
-- doGet (a :|: b) = (,) <$> doGet a <*> doGet b
-- doGet (Atom a)  = a

-- getf1 = F1 :$: getint :|: gettext

-- --versf2 = fromText :$: gettext

-- instance Versionable 1 Obj3 where
--   reader _ = doGet $ Resolve $ Obj3 :$: (fromInt :$: getint) :|: (fromText :$: gettext)
--   writer _ = undefined

-- instance Versionable 2 Obj3 where
--   reader _ = doGet $ _replaceAt 1 (fromText :$: gettext) $ geto3
--   writer _ = writer (Proxy :: Proxy 3)

-- geto3 = Obj3 :$: getf1 :|: getf2 F2

-- instance Versionable 3 Obj3 where
--   reader _ = doGet geto3
--   writer _
--     Obj3{..} = put f31 >> put f32

-- -- a bytestring for version 
-- bs1 :: ByteString
-- bs1 = runPut $ put (Obj1 12 "foo")

-- bs2 :: ByteString
-- bs2 = runPut $ put (Obj2 (F1 12 "bar") "foo")

