
{-
      Event Sourcing, Functionally
      ============================

 * A report on my experience building a system based on
   Event Sourcing, in Haskell, over the past few years
 * How a pure language can help us model a "business domain"
   yet provide strong technical guarantees
 * How a sophisticated type-system can help us build flexible
   and maintainable software

-}

{-
       Arnaud Bailly - arnaud@gorillaspace.co

 * 20 years experience in Java

 * Been writing Haskell code since 2001

 * Repented agile coach

 * Now CTO at https://gorillaspace.co to change how companies
   manage office space
-}

-- * Imports, stuff to make the compiler happy

-- {{{
{-# LANGUAGE DataKinds, FlexibleInstances, InstanceSigs, MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables,
             TypeOperators #-}
module Hevents.Eff.Demo where

import           Hevents.Eff.Stuff
import           Prelude           hiding (id, init, (.))

  -- }}}

-- * Let's start writing a test...
-- We want to model a bounded counter

-- {{{

aCounter :: Spec
aCounter = describe "Counter Model" $ do

  it "should apply commands given they respect bounds" $
    property $ prop_shouldApplyCommandRespectingBounds

  it "should not apply commands over bounds" $
    property $ prop_shouldNotApplyCommandsOverBounds

  -- }}}

-- ** Some properties modelling our counter as an
--    Event Sourced entity
--
--  * `Counter` is a pure (e.g. side-effects free) model
--  * `act` issues `Command`s to the model,
--  * producing `Event`s which are `apply`ied to the model
--  * QuickCheck generates `Command` to test

  -- {{{

prop_shouldApplyCommandRespectingBounds :: Command Counter -> Bool
prop_shouldApplyCommandRespectingBounds c@(Increment n) =
  let OK result = init `act` c
  in  init `apply` result == Counter n
prop_shouldApplyCommandRespectingBounds c@(Decrement n) =
  let counter20 = Counter 20
      OK result = counter20 `act` c
  in  counter20 `apply` result == Counter (20 - n)

prop_shouldNotApplyCommandsOverBounds :: [ Command Counter ] -> Bool
prop_shouldNotApplyCommandsOverBounds commands =
  let finalCounter = counter $ execState (mapM updateModel commands) init
  in  finalCounter >= 0 && finalCounter <= 100

  -- }}}

-- ** Definition of the complete `Counter` model

-- *** Data types for our model
  -- {{{

newtype Counter = Counter { counter :: Int }
  deriving (Eq,Show)

type instance Command Counter = CCounter
type instance Event Counter = ECounter
type instance Error Counter = ErCounter

data CCounter  = Increment Int
               | Decrement Int
               deriving (Eq, Show)

data ECounter = Added Int deriving (Eq,Show)

data ErCounter  = OutOfBounds
                | SystemError Text
                deriving (Eq,Show)

  -- }}}

-- *** Implementation of `Command`s and `Event`s

  -- {{{

instance Model Counter where

  init = Counter 0
  act :: Counter -> Command Counter
      -> Result Counter
  Counter k `act` Increment n = if k + n <= 100
                                then OK $ Added n
                                else KO OutOfBounds

  Counter k `act` Decrement n = if k - n >= 0
                                then OK $ Added (-n)
                                else KO OutOfBounds

  apply :: Counter -> Event Counter
        -> Counter
  Counter k `apply` Added n = Counter $ k + n

  -- }}}

-- *** We need a way to generate `Arbitrary` instances
--     of `Command`s
  -- {{{

instance Arbitrary CCounter where
  arbitrary = oneof [ Increment <$> choose (0,20)
                    , Decrement <$> choose (0,20)
                    ]

  -- }}}

-- * We now have a fully functional event-sourced bounded counter *Model*
-- let's expose some services that end users could access...
--

-- ** Let's write tests modelling expected interactions with our service

-- *** Our interaction model is simple
--
-- Clients of our service can:
--
--  * increment the counter
--  * decrement the counter
--  * get the current value of the counter

  -- {{{

data CounterAction = GetCounter
                   | IncCounter Int
                   | DecCounter Int
                   deriving (Show)

instance Arbitrary CounterAction where
  -- we use frequency to represent some expected (or observed) behaviour
  -- our users' behaviour model could be much more complex...
  arbitrary = frequency [ (3, return GetCounter)
                        , (2, IncCounter <$> choose (0,10))
                        , (1, DecCounter <$> choose (0,10))
                        ]

  -- }}}

-- *** We want to ensure no sequence of commands breaks
--     counter's bounds

  -- {{{

prop_servicesRespectCounterBounds :: [ CounterAction ] -> Property
prop_servicesRespectCounterBounds actions = monadicIO $ do
  -- given some underlying storage...
  results <- run $ withStorage defaultOptions $ \ storage -> do
    -- ... and an initial model ...
    model <- prepareModel storage
    -- ... for all actions in the sequence ...
    forM actions
      -- ... interpret the action to have some effect on our model ...
      (interpret
      -- ... then apply effect using given storage
       >>> effect model)

  -- check all valid results respect bounds
  assert $ all (\c -> c >= 0 && c <= 100) (rights results)

  -- }}}

-- ** Implement an effectful service on top of a pure model

-- *** `EventSourced` service is defined as the composition of
-- several `Functor`s lifted to a ''Free Monad'', e.g. several
-- "languages"

  -- {{{

type EventSourced m a =
  Eff
  -- Service maintains some parameterized `State`
  (State m
   -- It can raise exceptions
   :> Exc ServantErr
   -- It can run arbitrary IO actions
   :> Lift IO
   -- It can do nothing
   :> Void)
  -- returning some result
  a

-- An `effect` is the composition of various "handlers" for each
-- composed `Functor`
effect storage = runLift . runExc . runState storage

prepareModel storage = do
  makePersist (init :: Counter) storage systemError
  where
    systemError (IOError t) = SystemError t

  -- }}}

-- *** Interpretation of actions is straightforward

  -- {{{

interpret GetCounter     = getCounter
interpret (IncCounter n) = increment n
interpret (DecCounter n) = decrement n

getCounter :: EventSourced Counter Int
getCounter = counter <$> getState

increment :: Int -> EventSourced Counter Int
increment n = applyCommand (Increment n) >>= handleResult

decrement :: Int -> EventSourced Counter Int
decrement n = applyCommand (Decrement n) >>= handleResult

handleResult :: Either ErCounter ECounter
             -> EventSourced Counter Int
handleResult = either
  (throwExc . fromModelError)
  (const $ counter <$> getState)
  where
    fromModelError e = err400 { errBody = toLazyByteString $ stringUtf8 $ "Invalid command " ++ show e }

-- This is needed to ensure proper persistence of events
instance Serialize ECounter where
  put (Added i) = put i
  get           = Added <$> get

-- Being `Versionable` means we can evolve our Model's Events incrementally
instance Versionable ECounter

  -- }}}

-- * Let's expose our counter services through a REST API

-- ** Servant provides type-safe APIs definition
  -- {{{

type CounterApi =
  "counter" ::>
  (Get '[JSON] Int
    :<|> "increment" ::> Capture "inc" Int ::> Get '[JSON] Int
    :<|> "decrement" ::> Capture "dec" Int ::> Get '[JSON] Int)

counterApi :: Proxy CounterApi
counterApi = Proxy

  -- }}}

-- ** Let's write a test for our API against actual services
-- We use the same client-centric actions definitions than before

  -- {{{

prop_counterServerImplementsCounterApi
  :: [ CounterAction ] -> Property
prop_counterServerImplementsCounterApi actions = monadicIO $ do
  let baseUrl = BaseUrl Http "localhost" 8082 ""
  results <- run $ withStorage defaultOptions $ \ storage -> do
    mgr <- newManager defaultManagerSettings
    model <- prepareModel storage
    server <- runWebServerErr 8082 counterApi
              (Nat $ ExceptT . effect model) handler
    mapM (interpretAPI $ ClientEnv mgr baseUrl) actions
      `finally` cancel server

  assert $ all (\c -> c >= 0 && c <= 100) results

  -- }}}

-- *** Actions are interpreted as REST calls from an HTTP client

  -- {{{

interpretAPI env GetCounter     =
  rethrowError =<< runClientM (counterState) env
interpretAPI env (IncCounter n) =
  rethrowError =<< runClientM (incCounter n) env
interpretAPI env (DecCounter n) =
  rethrowError =<< runClientM (decCounter n) env

rethrowError = either throwIO return

counterState :<|> incCounter :<|> decCounter = client counterApi

  -- }}}

-- *** Server-side handler is the composition of already defined service handlers
  -- {{{

handler = getCounter :<|> increment :<|> decrement

  -- }}}

-- * Main server
  -- {{{

main :: IO ()
main = do
  [port] <- getArgs
  withStorage defaultOptions $ \ storage -> do
    model <- prepareModel storage
    server <- runWebServerErr (Prelude.read port) counterApi (Nat $ ExceptT . effect model) handler
    wait server

  -- }}}

{-

 * Code is available online: https://github.com/abailly/hevents

 * There are slides:
   http://abailly.github.io/slides/life-beyond-relational-db.html

 * This is still a work in progress. There are lots of areas
   I would like to explore...

 * Replicated and strongly consistent backend, aka. Raft
 * Cryptographically signed ledger to store events,
   aka. blockchain
 * Improved boostrapping and boilerplate handling

-}
