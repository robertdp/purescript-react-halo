module React.Halo.Component.State where

import Prelude
import Data.Map (Map)
import Data.Map as Map
import Effect (Effect)
import Effect.Aff (Aff, Fiber)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import React.Halo.Component (Spec)
import React.Halo.Component.Control (ForkId, SubscriptionId)

newtype StateRef props state action
  = StateRef (Ref (State props state action))

type State props state action
  = { component :: Spec props state action Aff
    , props :: props
    , state :: state
    , fresh :: Ref Int
    , subscriptions :: Ref (Map SubscriptionId (Effect Unit))
    , forks :: Ref (Map ForkId (Fiber Unit))
    }

makeState :: forall props state action. Spec props state action Aff -> props -> state -> Effect (StateRef props state action)
makeState component props state = do
  fresh' <- Ref.new 0
  subscriptions <- Ref.new Map.empty
  forks <- Ref.new Map.empty
  stateRef <- Ref.new { component, props, state, fresh: fresh', subscriptions, forks }
  pure (StateRef stateRef)

fresh :: forall props state action a. (Int -> a) -> StateRef props state action -> Effect a
fresh f (StateRef stateRef) = do
  state <- Ref.read stateRef
  Ref.modify' (\a -> { state: a + 1, value: f a }) state.fresh
