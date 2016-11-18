{-# LANGUAGE DeriveFunctor, FlexibleContexts, MultiParamTypeClasses, TupleSections, TypeOperators #-}
{-| An effect for logging -}
module Hevents.Eff.Log where

import           Control.Arrow    (second)
import           Control.Eff
import           Control.Eff.Lift (Lift, lift)
import           Data.Typeable

-- |The type of effect for logging entries of type `entry`
data Log l a = Log { entry :: l,  next :: a }
  deriving (Functor, Typeable)

-- | a monadic action that does the real logging
type Logger m l = l -> m ()

-- | Log something.
logE :: (Typeable l, Member (Log l) r)
  => l -> Eff r ()
logE line = send $ inj $ Log line ()

-- | Collect log messages in a list
-- This is somewhat similar to <Writer https://hackage.haskell.org/package/extensible-effects-1.11.0.4/docs/src/Control-Eff-Writer-Lazy.html> effect
runLogPure :: (Typeable l)
              => Eff (Log l :> r) a -> Eff r (a, [l])
runLogPure = freeMap (return . (,[])) (\ u -> handleRelay u runLogPure performLog)
  where
    performLog (Log l k) = second (l:) <$> runLogPure k

-- | Run the 'Logger' action in the base monad for every log line.
runLog :: (Typeable l, Typeable m, SetMember Lift (Lift m) (Log l :> r))
          => Logger m l -> Eff (Log l :> r) a -> Eff r a
runLog logger = freeMap return (\ u -> handleRelay u (runLog logger) (runLog logger . performLog))
  where
    performLog (Log e k) = lift (logger e) >> k

