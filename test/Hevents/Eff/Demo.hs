{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
module Hevents.Eff.Demo where

-- * Imports, stuff to make the compiler happy

import           Control.Category
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import qualified Control.Eff                as E
import           Control.Eff.Exception
import           Control.Eff.Lift           as E hiding (lift)
import           Control.Exception          (finally)
import           Control.Monad.Except
import qualified Control.Monad.State        as ST
import           Control.Monad.Trans.Either
import qualified Data.ByteString.Builder    as BS
import           Data.Either                (rights)
import           Data.Proxy
import           Data.Serialize             (Serialize, get, put)
import           Data.Typeable
import           Data.Void
import           Hevents.Eff                as W
import           Prelude                    hiding (init, (.))
import           Servant
import           Servant.Client
import           System.Environment
import           Test.Hspec
import           Test.QuickCheck            as Q
import           Test.QuickCheck.Monadic    as Q

-- * Let's start writing a test...

aCounter :: Spec
aCounter = describe "Counter Model" $ do
  it "should apply events from commands given they respect bounds" $ property $
    prop_shouldApplyCommandRespectingBounds
  it "should not apply commands over bounds" $ property $
    prop_shouldNotApplyCommandsOverBounds


prop_shouldApplyCommandRespectingBounds :: Command Counter -> Bool
prop_shouldApplyCommandRespectingBounds c@(Increment n) = let OK result = init `act` c
                                                          in  init `apply` result == Counter n
prop_shouldApplyCommandRespectingBounds c@(Decrement n) = let counter20 = Counter 20
                                                              OK result = counter20 `act` c
                                                          in  counter20 `apply` result == Counter (20 - n)

prop_shouldNotApplyCommandsOverBounds :: [ Command Counter ] -> Bool
prop_shouldNotApplyCommandsOverBounds commands =
  let finalCounter = counter $ ST.execState (mapM updateModel commands) init
  in  finalCounter >= 0 && finalCounter <= 100

newtype Counter = Counter { counter :: Int } deriving (Eq,Show)

instance Model Counter where
  data Command Counter = Increment Int
                       | Decrement Int
                       deriving (Eq, Show)
  data Event Counter = Added Int deriving (Eq,Show)
  data Error Counter = OutOfBounds deriving (Eq,Show)

  init = Counter 0

  Counter k `act` Increment n = if k + n <= 100
                                then OK $ Added n
                                else KO OutOfBounds

  Counter k `act` Decrement n = if k - n >= 0
                                then OK $ Added (-n)
                                else KO OutOfBounds

  Counter k `apply` Added n = Counter $ k + n

instance Arbitrary (Command Counter) where
  arbitrary = oneof [ Increment <$> choose (0,20)
                    , Decrement <$> choose (0,20)
                    ]

-- * We now have a fully functional event-sourced bounded counter *Model*
-- let's expose some services that end users could access...
--
-- First write tests representing services interactions

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

prop_servicesRespectCounterBounds :: [ CounterAction ] -> Property
prop_servicesRespectCounterBounds actions = Q.monadicIO $ do
  results <- Q.run $ do
    (model, storage) <- prepareContext
    mapM (effect storage model . interpret) actions

  assert $ all (\c -> c >= 0 && c <= 100) (rights results)

-- this is where we define the initial state of our services and model
prepareContext = (,)           <$>
  newTVarIO (W.init :: Counter) <*>
  atomically W.makeMemoryStore

-- defines the language(s) in which we can express our services
effect :: (Typeable m, Typeable e, Storage STM s, Registrar STM m reg)
         => s -> reg
         -> E.Eff (State m E.:> Store E.:> Exc e E.:> Lift STM E.:> Void) a -> IO (Either e a)
effect s m = atomically . runSync . runExc . W.runStore s .  W.runState m

-- defines how to interpret our action model in terms of actual services

interpret GetCounter     = getCounter
interpret (IncCounter n) = increment n
interpret (DecCounter n) = decrement n

getCounter :: EventSourced Int
getCounter = counter <$> getState

increment :: Int -> EventSourced Int
increment n = applyCommand (Increment n) >>= storeEvent

decrement :: Int -> EventSourced Int
decrement n = applyCommand (Decrement n) >>= storeEvent

type EventSourced a = E.Eff (State Counter E.:> Store E.:> Exc ServantErr E.:> Lift STM E.:> Void) a

storeEvent :: Either (Error Counter) (Event Counter)
             -> EventSourced Int
storeEvent = either
  (throwExc . fromModelError)
  (either (throwExc . fromDBError) (const $ counter <$> getState) <=< store)
  where
    fromModelError e = err400 { errBody = BS.toLazyByteString $ BS.stringUtf8 $ "Invalid command " ++ show e }
    fromDBError    e = err500 { errBody = BS.toLazyByteString $ BS.stringUtf8 $ "DB Error " ++ show e }

instance Serialize (Event Counter) where
  put (Added i) = put i
  get           = Added <$> get

-- * Expose our counter services through a REST API

type CounterApi = "counter" :> (Get '[JSON] Int
                                :<|> "increment" :> Capture "inc" Int :> Get '[JSON] Int
                                :<|> "decrement" :> Capture "dec" Int :> Get '[JSON] Int)

counterApi :: Proxy CounterApi
counterApi = Proxy

-- * Let's write a test for our API against actual services, using user-centric actions
prop_counterServerImplementsCounterApi :: [ CounterAction ] -> Property
prop_counterServerImplementsCounterApi actions = Q.monadicIO $ do
  results <- Q.run $ do
    (model, storage) <- prepareContext
    server <- W.runWebServerErr 8082 counterApi (Nat $ EitherT . effect storage model) handler
    mapM runClient actions `finally` cancel server

  assert $ all (\c -> c >= 0 && c <= 100) (rights results)

runClient GetCounter     = runEitherT $ counterState
runClient (IncCounter n) = runEitherT $ incCounter n
runClient (DecCounter n) = runEitherT $ decCounter n

handler = getCounter :<|> increment :<|> decrement
