module React.Halo.Internal.State where

import Prelude
import Data.Map (Map)
import Data.Map as Map
import Effect (Effect)
import Effect.Aff (Aff, Fiber)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import React.Halo.Internal.Control (HaloM)
import React.Halo.Internal.Types (ForkId, Lifecycle, SubscriptionId)

-- | HThe alo component state used during evaluation.
newtype HaloState props state action
  = HaloState
  { eval :: Lifecycle props action -> HaloM props state action Aff Unit
  , render :: state -> Effect Unit
  , unmounted :: Ref Boolean
  , props :: Ref props
  , state :: Ref state
  , fresh :: Ref Int
  , subscriptions :: Ref (Map SubscriptionId (Effect Unit))
  , forks :: Ref (Map ForkId (Fiber Unit))
  }

-- | Creates a starting `HaloState`, ready for initialization.
createInitialState ::
  forall props state action.
  state ->
  (Lifecycle props action -> HaloM props state action Aff Unit) ->
  (state -> Effect Unit) -> props -> Effect (HaloState props state action)
createInitialState initialState eval render props' = do
  unmounted <- Ref.new false
  fresh' <- Ref.new 0
  props <- Ref.new props'
  state <- Ref.new initialState
  subscriptions <- Ref.new Map.empty
  forks <- Ref.new Map.empty
  pure $ HaloState { eval, render, unmounted, props, state, fresh: fresh', subscriptions, forks }

-- | Issue a new identifier, unique to this component.
fresh :: forall props state action a. (Int -> a) -> HaloState props state action -> Effect a
fresh f (HaloState s) = Ref.modify' (\a -> { state: a + 1, value: f a }) s.fresh
