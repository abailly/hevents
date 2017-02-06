{-# LANGUAGE DataKinds, GADTs, LambdaCase, PatternSynonyms, PolyKinds, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators,
             UndecidableInstances, ViewPatterns #-}

import           Control.Exception
import           Prelude           hiding (pure, (<*>))
import           System.IO

type s :-> t = forall (i :: k) . s (i :: k) -> t (i :: k)


class IFunctor (f :: (k -> *) -> (h -> *)) where
  imap :: (s :-> t) -> f s :-> f t

class IFunctor m => IMonad (m :: (k -> *) -> k -> *) where
  iskip :: p :-> m p
  iextend :: (p :-> m q) -> (m p :-> m q)


iseq :: IMonad m => (p :-> m q) -> (q :-> m r) -> p :-> m r
iseq f g = iextend g . f

(?>=) :: IMonad m => m p (i :: k) -> (p :-> m q) -> m q (i :: k)
c ?>= f = iextend f c

(=>=) :: IMonad m => m (a := j) i -> (a -> m q j) -> m q i
c =>= f = c ?>= \ (V a) -> f a

data (:=) :: * -> x -> x -> * where
  V :: a -> (a := k) k

type Atkey m i j a = m (a := j) i

ireturn :: IMonad m => a -> Atkey m i i a
ireturn a = iskip (V a)

data State = Open | Closed

data IState (s :: State) where
  IOpen :: IState 'Open
  IClosed :: IState 'Closed

trans :: IState i -> IState i
trans IOpen   = IOpen
trans IClosed = IClosed

data (p :>> q) r i = p i :& (q :-> r)

instance IFunctor (p :>> q) where
  imap h (p :& k) = p :& (h . k)

data (f :+: g) p i = InL (f p i) | InR (g p i)

instance (IFunctor f, IFunctor g) => IFunctor (f :+: g) where
  imap h (InL f) = InL (imap h f)
  imap h (InR g) = InR (imap h g)

infixl 7 :>>
infixr 6 :+:
infixl 8 :=

type FH = FilePath := 'Closed :>> IState
      :+: ()       := 'Open   :>> Maybe Char := 'Open
      :+: ()       := 'Open   :>> ()         := 'Closed

data (:*) p k i where
  Ret :: p i          -> (f :* p) i
  Do  :: f (f :* p) i -> (f :* p) i

instance IFunctor f => IMonad ((:*) f) where
  iskip = Ret
  iextend g (Ret p)  = g p
  iextend g (Do ffp) = Do (imap (iextend g) ffp)

instance IFunctor f => IFunctor ((:*) f) where
  imap f = iextend (iskip . f)

-- does not compile...
-- pattern FOpen p k = Do (InL (V p :& k))
-- pattern FGetC   k = Do (InR (InL (V () :& k)))
-- pattern FClose  k = Do (InR (InR (V () :& k)))


pattern FOpen fp k = Do (InL (V fp :& k))

fOpen :: FilePath -> (FH :* IState) 'Closed
fOpen fp = FOpen fp Ret

fGetC :: (FH :* (Maybe Char := 'Open)) 'Open
fGetC = Do (InR (InL (V () :& Ret)))

fClose :: (FH :* (() := 'Closed)) 'Open
fClose = Do (InR (InR (V () :& Ret)))

runFH :: (FH :* (a := 'Closed)) 'Closed -> IO a
runFH (Ret (V a))  = return a
runFH (FOpen fp k) = catch
  (openFile fp ReadMode >>= openFH (k IOpen))
  (\ (_ :: IOException) -> runFH (k IClosed))
  where
    openFH :: (FH :* (a := 'Closed)) 'Open -> Handle -> IO a
    openFH (Do (InR (InR (V () :& k)))) h = hClose h >> runFH (k (V ()))
    openFH (Do (InR (InL (V () :& k)))) h = catch
      (hGetChar h >>= \c -> openFH (k (V (Just c))) h)
      (\ (_ :: IOException) -> openFH (k (V Nothing)) h)

class IFunctor m => IApplicative (m :: (i -> *) -> i -> *) where
  pure :: x -> Atkey m k k x
  (<*>) :: Atkey m h j (s -> t) -> Atkey m j k s -> Atkey m h k t

instance IFunctor f => IApplicative ((:*) f) where
  pure = ireturn
  mf <*> ms = mf =>= \ f -> ms =>= \ s -> ireturn (f s)

readOpenFile :: (FH :* (String := 'Open)) 'Open
readOpenFile = fGetC =>= \ x -> case x of
                                  Nothing -> pure ""
                                  Just c  -> pure (c :) <*> readOpenFile

fileContents :: FilePath -> (FH :* (Maybe String := 'Closed)) 'Closed
fileContents p = fOpen p ?>= \ b -> case b of
                                    IClosed -> pure Nothing
                                    IOpen   -> (pure Just <*> readOpenFile) =>= \ s -> fClose =>= const (ireturn s)

data (:^) :: ((i -> *) -> i -> *) -> (i -> *) -> i -> * where
  RET :: s i -> (m :^ s) i
  DO :: (forall t . (s :-> (m :^ t)) -> m (m :^ t) i) -> (m :^ s) i

instance IFunctor ((:^) m) where
  imap f (RET s) = RET (f s)
  imap f (DO g ) = DO  (\ k -> g (k . f))

instance IMonad ((:^) m) where
  iskip = RET
  iextend f (RET s) = f s
  iextend f (DO g)  = DO (\ k -> g (iextend k . f))

instance IFunctor f => IApplicative ((:^) f) where
  pure = ireturn
  mf <*> ms = mf =>= \ f -> ms =>= \ s -> ireturn (f s)

thunk :: IFunctor f => Either (f (f :^ t) i) (t i) -> (f :^ t) i
thunk (Right t)  = RET t
thunk (Left fft) = DO (\ k -> imap (iextend k) fft)


pattern FRet a = Right (V a)

pattern FOPEN:: t5 -> t4 :-> t2 -> Either (((t5 := t6 :>> t4) :+: r) t2 t6) t
pattern FOPEN p k = Left (InL (V p :& k))

pattern FCLOSE ::  t4 :-> t2 -> Either ((l :+: l' :+: ((() := t6 :>> t4))) t2 t6) t
pattern FCLOSE  k = Left (InR (InR (V () :& k)))

pattern FGETC ::  t4 :-> t2 -> Either ((l :+: ((() := t6 :>> t4)) :+: r) t2 t6) t
pattern FGETC  k = Left (InR (InL (V () :& k)))

fOPEN :: FilePath -> (FH :^ IState) 'Closed
fOPEN p = thunk (FOPEN p RET)

fGETC :: (FH :^ (Maybe Char := 'Open)) 'Open
fGETC = thunk (Left (InR (InL (V () :& RET))))

fCLOSE :: (FH :^ (() := 'Closed)) 'Open
fCLOSE = thunk (Left (InR (InR (V () :& RET))))

force :: (f :^ t) i -> Either (f (f :^ t) i) (t i)
force (RET t) = Right t
force (DO k)  = Left (k RET)

runFH' :: (FH :^ (a := 'Closed)) 'Closed -> IO a
runFH' (force -> FRet a)    = return a
runFH' (force -> FOPEN s k) = catch
  (openFile s ReadMode >>= openFH (k IOpen))
  (\ (_ :: IOException) -> runFH' (k IClosed))
  where
    openFH :: (FH :^ (a := 'Closed)) 'Open -> Handle -> IO a
    openFH (force -> FCLOSE k) h = hClose h >> runFH' (k (V ()))
    openFH (force -> FGETC k) h = catch
      (hGetChar h >>= \ c -> openFH (k (V (Just c))) h)
      (\ (_ :: IOException) -> openFH (k (V Nothing)) h)

fileContents' :: FilePath -> (FH :^ (Maybe String := 'Closed)) 'Closed
fileContents' fp = fOPEN fp ?>=
                   \ case
                     IClosed -> pure Nothing
                     IOpen   -> (pure Just <*> readOpenFile') =>= \ s -> fCLOSE =>= const (ireturn s)

readOpenFile' :: (FH :^ (String := 'Open)) 'Open
readOpenFile' = fGETC =>= \ x -> case x of
                                  Nothing -> pure ""
                                  Just c  -> pure (c :) <*> readOpenFile'
