{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TypeOperators #-}
{-|
Provides <fast-logger https://hackage.haskell.org/package/fast-logger-2.4.7> based implementation of `Logger`
to be used in `Log` effect
-}
module Hevents.Eff.Log.Fast(runLogStdout, runLogFile, runLogWithLoggerSet,
                            module System.Log.FastLogger) where

import           Control.Eff
import           Control.Eff.Lift
import           Data.Monoid
import           Data.Typeable
import           Hevents.Eff.Log
import           System.Log.FastLogger

-- | Log to file.
runLogFile :: (Typeable l, ToLogStr l, SetMember Lift (Lift IO) r, SetMember Lift (Lift IO) (Log l :> r))
  => FilePath -> proxy l -> Eff (Log l :> r) a -> Eff r a
runLogFile f proxy eff = do
    s <- lift $ newFileLoggerSet defaultBufSize f
    runLogWithLoggerSet s proxy eff <* lift (flushLogStr s)

-- | Log to stdout.
runLogStdout :: (Typeable l, ToLogStr l, SetMember Lift (Lift IO) (Log l :> r), SetMember Lift (Lift IO) r)
  => proxy l -> Eff (Log l :> r) a -> Eff r a
runLogStdout proxy eff = do
    s <- lift $ newStdoutLoggerSet defaultBufSize
    runLogWithLoggerSet s proxy eff <* lift (flushLogStr s)

runLogWithLoggerSet :: (Typeable l, ToLogStr l, SetMember Lift (Lift IO) (Log l :> r))
  => LoggerSet -> proxy l -> Eff (Log l :> r) a -> Eff r a
runLogWithLoggerSet s _ = runLog (loggerSetLogger s)

loggerSetLogger :: ToLogStr l => LoggerSet -> Logger IO l
loggerSetLogger loggerSet = pushLogStr loggerSet . (<> "\n") . toLogStr
