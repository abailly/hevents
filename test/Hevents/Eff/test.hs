{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes    #-}

import           Control.Comonad
import           Data.Functor.Identity

newtype Pretext s a = Pretext { runPretext :: forall f. (Functor f) => (s -> f s) -> f a } deriving (Functor)

experiment :: Functor f => (s -> f s) -> Pretext s a -> f a
experiment f (Pretext k) = k f

-- /show
main = putStrLn "It typechecks, so it must be correct!"

instance Comonad (Pretext s) where
  extract p@(Pretext k) = runIdentity (experiment k p)
