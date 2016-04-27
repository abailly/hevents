module Hevents.Eff.Store.Storage where

-- | Generic interface for low-level `Storage` engines.
class Storage s where
  store :: Serialize a => a      -> s ()
  load  :: Serialize a => Offset -> Count -> s [a]
