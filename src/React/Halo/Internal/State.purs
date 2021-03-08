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
newtype HaloState context state action
  = HaloState
  { eval :: Lifecycle context action -> HaloM context state action Aff Unit
  , update :: state -> Effect Unit
  , finalized :: Ref Boolean
  , context :: Ref context
  , state :: Ref state
  , fresh :: Ref Int
  , subscriptions :: Ref (Map SubscriptionId (Effect Unit))
  , forks :: Ref (Map ForkId (Fiber Unit))
  }

-- | Creates a starting `HaloState`, ready for initialization.
createInitialState ::
  forall context state action.
  { context :: context
  , state :: state
  , eval :: Lifecycle context action -> HaloM context state action Aff Unit
  , update :: state -> Effect Unit
  } ->
  Effect (HaloState context state action)
createInitialState spec@{ eval, update } = do
  finalized <- Ref.new false
  fresh' <- Ref.new 0
  context <- Ref.new spec.context
  state <- Ref.new spec.state
  subscriptions <- Ref.new Map.empty
  forks <- Ref.new Map.empty
  pure $ HaloState { eval, update, finalized, context, state, fresh: fresh', subscriptions, forks }

-- | Issue a new identifier, unique to this component.
fresh :: forall context state action a. (Int -> a) -> HaloState context state action -> Effect a
fresh f (HaloState s) = Ref.modify' (\a -> { state: a + 1, value: f a }) s.fresh
