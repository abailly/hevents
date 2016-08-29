-- A `Model` is the core unit of work in an *Event Sourced* application. Using traditional
-- DDD terminology, a `Model` is made of:

-- * An `Aggregate` which is a rooted graph of `Entities` for which we need to maintain state and which
--   represent the state of the business domain,
-- * A language of `Command`s which are *actions* users of the system can do on the `Aggregate`,
-- * A language of `Event`s which affect the `Aggregate` and whose sequence *is* the state of the system of
--   which current state of aggregate is only a representation.

module Hevents.Eff.Model where

import Control.Monad.State

class Model a where

-- The type of events that can affect this `Model`. Technically this is a *type family* which is equivalent to
-- parameterizing the `Model` class. We need a `data` type family to prevent typechecking to fail when we try
-- to relate different type-families for a single value of `a` due to type families not being injective (See https://ghc.haskell.org/trac/ghc/wiki/InjectiveTypeFamilies
-- for discussion on this feature which will be available in GHC 8.0.

  data Event a   :: *

-- The type of commands this `Model` understands and which users can send to it.

  data Command a :: *

-- The type of errors execution of a `Command a` can produce. Contrary to `Event`s results, `Error`s are not
-- part of the persistence model of the aggregate: They are transient.

  data Error a   :: *

-- Initial (empty) state of the `Aggregate`

  init :: a

-- `act` try to execute some `Command a` taking into account current state of the aggregate, producing a `Result` which
-- can be either an `Error` or an `Event`.

  act :: a -> Command a -> Result a

-- `apply` transitions an aggregate's state with some event. Events are not expected to *fail* as they represent the *past*
-- of the aggregate, what occured to it (hence they are usually named using past tense) rather than some *future*. Applying
-- events in an arbitrary can yield to broken state of model.

  apply :: a -> Event a -> a

-- The result of applying a `Command` to a `Model`. This is basically `Either` in disguise...

data Result a = KO (Error a)
              | OK (Event a)

-- Helper function to run a command and immediately apply the result on model, returning updated state and result.
-- This builds a `State` instance that can be used.

updateModel :: (Model a) => Command a -> State a (Result a)
updateModel command = state actAndApply
  where
    actAndApply s = case commandResult of
      OK e -> (commandResult, s `apply` e)
      KO _ -> (commandResult, s)
      where
        commandResult = s `act` command
