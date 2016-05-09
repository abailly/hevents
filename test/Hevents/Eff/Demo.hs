{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
module Hevents.Eff.Demo where

import           Control.Category
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import qualified Control.Eff                as E
import           Control.Eff.Lift           as E hiding (lift)
import           Control.Exception          (finally)
import           Control.Monad.Except
import qualified Control.Monad.State        as ST
import           Control.Monad.Trans.Either
import           Data.Either                (rights)
import           Data.Proxy
import           Data.Serialize             (Serialize, get, put, runGet)
import           Data.Typeable
import           Data.Void
import           Hevents.Eff                as W
import           Network.Wai.Handler.Warp   as W
import           Prelude                    hiding (init, (.))
import           Servant
import           Servant.Client
import           Test.Hspec
import           Test.QuickCheck            as Q
import           Test.QuickCheck.Monadic    as Q

-- * Define REST Interface

type CounterApi = "counter" :> Get '[JSON] Int
                  :<|> "counter" :> "increment" :> Capture "inc" Int :> Get '[JSON] Int
                  :<|> "counter" :> "decrement" :> Capture "dec" Int :> Get '[JSON] Int

counterApi :: Proxy CounterApi
counterApi = Proxy

-- * Minimal implementation

counterServer :: Server CounterApi
counterServer = getCounter
  :<|> increment
  :<|> decrement
  where
    getCounter = return 0
    increment  = const undefined
    decrement  = const undefined

-- * A Test
apiSpec :: Spec
apiSpec = describe "Counter Server" $ do
  it "should return 0 as initial state of counter" $ do
    s <- async $ W.run 8082 $ serve counterApi counterServer
    Right n <- (runEitherT $ getCounter) `finally` cancel s

    n `shouldBe` 0
      where
        getCounter :<|> _  :<|> _ = client counterApi (BaseUrl Http "localhost" 8082)

-- * The counter model
-- A bounded counter

instance Model Counter where
  data Command Counter = Increment Int | Decrement Int deriving (Eq,Show)
  data Event Counter = Added Int deriving (Eq,Show)
  data Error Counter = OutOfBounds deriving (Eq, Show)

  init = 0

  Counter n `act`   Increment i = if (n + i) > 100
                                  then KO OutOfBounds
                                  else OK $ Added i
  Counter n `act`   Decrement i = if (n - i) < 0
                                  then KO OutOfBounds
                                  else OK $ Added (-i)
  Counter n `apply` Added a     = Counter (n + a)

newtype Counter = Counter { counter :: Int } deriving (Eq, Show, Num)

-- * Test Counter properties

instance Arbitrary (Command Counter) where
  arbitrary = oneof [ Increment <$> number
                    , Decrement <$> number
                    ]
    where
      number = choose (0,10)

prop_shouldActAndApplyCommandsRespectingBounds :: Command Counter -> Bool
prop_shouldActAndApplyCommandsRespectingBounds c@(Increment i) = let OK result = init `act` c
                                                                 in init `apply` result == Counter i
prop_shouldActAndApplyCommandsRespectingBounds c@(Decrement i) = let counter = Counter 10
                                                                     OK result = counter `act` c
                                                                 in counter `apply` result == Counter (10 - i)

prop_shouldNotApplyCommandsOverBounds :: [Command Counter] -> Bool
prop_shouldNotApplyCommandsOverBounds commands = let finalCounter = counter $ ST.execState (mapM_ updateModel commands) init
                                                 in  finalCounter >= 0 && finalCounter <= 100

counterSpec :: Spec
counterSpec = describe "Counter model" $ do
  it "should apply result of commands given it respects bounds" $ property $ prop_shouldActAndApplyCommandsRespectingBounds
  it "should not allow applying commands out of bounds"         $ property $ prop_shouldNotApplyCommandsOverBounds


-- * Events Persistence

instance Serialize (Event Counter) where
  put (Added i) = put i
  get           = Added <$> get

asDBError OutOfBounds = IOError "out of bounds"

prop_persistEventsOnCounterModel :: [ Command Counter ] -> Property
prop_persistEventsOnCounterModel commands = Q.monadicIO $ do
  storage <- initialiseStorage
  let
    acts :: E.Eff (Store E.:> State Counter E.:> r) Counter
    acts = do
      _ <- sequence $ map (either (return . Left . asDBError) store <=< applyCommand) commands
      getState
  m <- Q.run $ atomically $ newTVar (W.init :: Counter) >>= \ m -> (runSync . runState m . runStore storage) acts
                                                                  `catchSTM` (\ (SyncException _) -> readTVar m)
  stored :: [ Event Counter ] <- reverse <$> (Q.run $ atomically $ (rights . map (runGet get)) <$> readMemoryStore storage)

  let storedVal = foldl apply W.init $ stored

  assert $ m == storedVal

  where
    readMemoryStore = readTVar . mem
    initialiseStorage = Q.run $ atomically $ W.makeMemoryStore


storeSpec :: Spec
storeSpec = describe "Events Storage" $ do
    it "should persist events applied to model" $ property $ prop_persistEventsOnCounterModel

-- * Complete Counter Server

data CounterApiAction = GetCounter
                      | IncCounter Int
                      | DecCounter Int
                      deriving (Show)

instance Arbitrary CounterApiAction where
  arbitrary = oneof [ return GetCounter
                    , IncCounter <$> choose (0,10)
                    , DecCounter <$> choose (0,10)
                    ]

effect :: (Typeable m, Storage STM s, Registrar STM m reg)
         => s -> reg
         -> E.Eff (State m E.:> Store E.:> Lift STM E.:> Void) :~> IO
effect s m = Nat $ atomically . runSync . W.runStore s .  W.runState m

handler = getCounter :<|> increment :<|> decrement
  where
    getCounter = counter <$> getState

    increment n = do
      r <- applyCommand (Increment n)
      void $ either (return . Left . asDBError) store r
      counter <$> getState

    decrement n = do
      r <- applyCommand (Decrement n)
      void $ either (return . Left . asDBError) store r
      counter <$> getState

prepareContext = (,) <$>
  newTVarIO (W.init :: Counter) <*>
  atomically W.makeMemoryStore

prop_counterServerImplementsCounterApi :: [ CounterApiAction ] -> Property
prop_counterServerImplementsCounterApi actions = Q.monadicIO $ do
  results <- Q.run $ do
    (model, storage) <- prepareContext
    server <- W.runWebServer 8082 counterApi (effect storage model) handler
    mapM runClient actions `finally` cancel server

  assert $ all withinBounds (rights results)

    where
      withinBounds n = n >= 0 && n <= 100


      getCounter :<|> incCounter :<|> decCounter = client counterApi (BaseUrl Http "localhost" 8082)

      runClient GetCounter     = runEitherT $ getCounter
      runClient (IncCounter n) = runEitherT $ incCounter n
      runClient (DecCounter n) = runEitherT $ decCounter n

serverSpec :: Spec
serverSpec = describe "Counter Server" $ do
  it "implements counter API with bounds" $ property $ prop_counterServerImplementsCounterApi

-- * Main app

main :: IO ()
main = do
  (model, storage) <- prepareContext
  W.runWebServer 8082 counterApi (effect storage model) handler >>= wait
