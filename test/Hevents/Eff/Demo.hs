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
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Builder    as BS
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

-- * First property to drive our counter model
counterSpec :: Spec
counterSpec = describe "Counter model" $ do
  it "should apply result of commands given it respects bounds" $ property $ prop_shouldActAndApplyCommandsRespectingBounds
  it "should not allow applying commands out of bounds"         $ property $ prop_shouldNotApplyCommandsOverBounds

-- * The commands that can act on the model
instance Arbitrary (Command Counter) where
  arbitrary = oneof [ Increment <$> number
                    , Decrement <$> number
                    ]
    where
      number = choose (0,20)

-- * First property, test all commands against pristine model
-- we can do
--
-- >>>> verboseCheck prop_shouldActAndApplyCommandsRespectingBounds
--
-- to see what's going on with generated values...
prop_shouldActAndApplyCommandsRespectingBounds :: Command Counter -> Bool
prop_shouldActAndApplyCommandsRespectingBounds c@(Increment i) = let OK result = init `act` c
                                                                 in init `apply` result == Counter i
prop_shouldActAndApplyCommandsRespectingBounds c@(Decrement i) = let counter = Counter 20
                                                                     OK result = counter `act` c
                                                                 in counter `apply` result == Counter (20 - i)

-- * A more interesting property, involving bounds of model

prop_shouldNotApplyCommandsOverBounds :: [Command Counter] -> Bool
prop_shouldNotApplyCommandsOverBounds commands = let finalCounter = counter $ ST.execState (mapM updateModel commands) init
                                                 in  finalCounter >= 0 && finalCounter <= 100

-- * The Counter model

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


instance Serialize (Event Counter) where
  put (Added i) = put i
  get           = Added <$> get


-- * Testing counter-based services
-- We are writing tests for higher-level services representing user interactions with
-- our basic model

data CounterAction = GetCounter
                   | IncCounter Int
                   | DecCounter Int
                   deriving (Show)

instance Arbitrary CounterAction where
  arbitrary = frequency [ (3, return GetCounter)
                        , (2, IncCounter <$> choose (0,10))
                        , (1, DecCounter <$> choose (0,10))
                        ]

prop_servicesRespectCounterBounds :: [ CounterAction ] -> Property
prop_servicesRespectCounterBounds actions = Q.monadicIO $ do
  results <- Q.run $ do
    (model, storage) <- prepareContext
    mapM (effect storage model . interpret) actions

  assert $ all withinBounds (rights results)

    where
      withinBounds n = n >= 0 && n <= 100

      interpret GetCounter     = getCounter
      interpret (IncCounter n) = increment n
      interpret (DecCounter n) = decrement n

getCounter :: EventSourced Int
getCounter = counter <$> getState

increment :: Int -> EventSourced Int
increment n = applyCommand (Increment n) >>= storeEvent

decrement :: Int -> EventSourced Int
decrement n = applyCommand (Decrement n) >>= storeEvent

effect :: (Typeable m, Typeable e, Storage STM s, Registrar STM m reg)
         => s -> reg
         -> E.Eff (State m E.:> Store E.:> Exc e E.:> Lift STM E.:> Void) a -> IO (Either e a)
effect s m = atomically . runSync . runExc . W.runStore s .  W.runState m

prepareContext = (,) <$>
  newTVarIO (W.init :: Counter) <*>
  atomically W.makeMemoryStore

type EventSourced a = E.Eff (State Counter E.:> Store E.:> Exc ServantErr E.:> Lift STM E.:> Void) a

storeEvent :: Either (Error Counter) (Event Counter)
             -> EventSourced Int
storeEvent = either
  (throwExc . fromModelError)
  (either (throwExc . fromDBError) (const $ counter <$> getState) <=< store)
  where
    fromModelError e = err400 { errBody = BS.toLazyByteString $ BS.stringUtf8 $ "Invalid command " ++ show e }
    fromDBError    e = err500 { errBody = BS.toLazyByteString $ BS.stringUtf8 $ "DB Error " ++ show e }

-- * REST API to access counter services

type CounterApi = "counter" :> Get '[JSON] Int
                  :<|> "counter" :> "increment" :> Capture "inc" Int :> Get '[JSON] Int
                  :<|> "counter" :> "decrement" :> Capture "dec" Int :> Get '[JSON] Int

counterApi :: Proxy CounterApi
counterApi = Proxy
