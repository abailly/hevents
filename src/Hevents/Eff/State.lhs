Defines an effect to synchronize changes to a `Model`, following the [Freer](http://okmij.org/ftp/Haskell/extensible/more.pdf) and extensible monad
paper and library.

> {-# LANGUAGE DeriveFunctor #-}
> {-# LANGUAGE GADTs         #-}
> module Hevents.Eff.State(State(..), Registrar(..), applyCommand, getState, runState) where
> 
> import           Control.Eff
> import           Control.Eff.Lift
> import           Data.Typeable
> import           Hevents.Eff.Model hiding (init)
> import           Prelude hiding (init)


A `Registrar` is responsible for the machinery needed to apply commands, maintain and retrieve state

> class (Monad m, Model s) => Registrar m s g  where
>   update :: (SetMember Lift (Lift m) r, Member (State s) r) =>  g -> State s (Eff r a) -> Eff r a
>   

A `State` is parameterized by the type of `Model` it manages.

> data State m a where

Atomically apply  a given `Command` to given state and updates it, returning the result of the command
execution: `Either` and `Error` or an `Event`.

>   ApplyCommand :: (Model m) => Command m -> (Either (Error m) (Event m) -> a) -> State m a

Retrieve the current state of the model

>   GetState     :: (Model m) =>                      (m                 -> a) -> State m a
>   deriving (Typeable)

Boilerplate code to:

* Make `State m` a `Functor,
* Provide "smart" constructors.

> instance Functor (State m) where
>   f `fmap` (ApplyCommand c k) = ApplyCommand c (f . k)
>   f `fmap` (GetState  k)      = GetState (f . k)
> 
> applyCommand :: (Model m, Typeable m, Member (State m) r)
>                  => Command m -> Eff r (Either (Error m) (Event m))
> applyCommand c = send $ inj $ ApplyCommand c id
> 
> getState :: (Model m, Typeable m, Member (State m) r) => Eff r m
> getState = send $ inj $ GetState id

Effective computation on a `State m` which is delegated to the given `Registrar`.

> runState :: (Typeable m, SetMember Lift (Lift mo) (State m :> r), Registrar mo m reg) => reg -> Eff (State m :> r) w -> Eff r w
> runState reg = freeMap return (\ u -> handleRelay u (runState reg) (runState reg . update reg))

