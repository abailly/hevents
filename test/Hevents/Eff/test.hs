{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes    #-}

import           Control.Comonad
import           Data.Functor.Identity
import           Network.Wai

data Store s a = Store s (s -> a)

instance Functor (Store s) where
  fmap f (Store s k) = Store s (f . k)

instance Comonad (Store s) where
  extract (Store s k) = k s
  duplicate (Store s k) = Store s (flip Store k)

newtype Pretext s a =
  Pretext { runPretext :: forall f. (Functor f) => (s -> f s) -> f a }
  deriving (Functor)

experiment :: Functor f => (s -> f s) -> Pretext s a -> f a
experiment f (Pretext k) = k f


instance Comonad (Pretext s) where
  extract     (Pretext k) = runIdentity $ k Identity
  duplicate p@(Pretext k) = Pretext $ \ f -> (p $>) `fmap` k f

-- /show
main = putStrLn "It typechecks, so it must be correct!"

