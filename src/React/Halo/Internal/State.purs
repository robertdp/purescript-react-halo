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
newtype HaloState props context state action
  = HaloState
  { eval :: Lifecycle props context action -> HaloM props context state action Aff Unit
  , update :: state -> Effect Unit
  , finalized :: Ref Boolean
  , props :: Ref props
  , context :: Ref context
  , state :: Ref state
  , fresh :: Ref Int
  , subscriptions :: Ref (Map SubscriptionId (Effect Unit))
  , forks :: Ref (Map ForkId (Fiber Unit))
  }

-- | Creates a starting `HaloState`, ready for initialization.
createInitialState ::
  forall props context state action.
  { props :: props
  , state :: state
  , context :: context
  , eval :: Lifecycle props context action -> HaloM props context state action Aff Unit
  , update :: state -> Effect Unit
  } ->
  Effect (HaloState props context state action)
createInitialState spec@{ eval, update } = do
  finalized <- Ref.new false
  fresh' <- Ref.new 0
  props <- Ref.new spec.props
  context <- Ref.new spec.context
  state <- Ref.new spec.state
  subscriptions <- Ref.new Map.empty
  forks <- Ref.new Map.empty
  pure $ HaloState { eval, update, finalized, props, context, state, fresh: fresh', subscriptions, forks }

-- | Issue a new identifier, unique to this component.
fresh :: forall props context state action a. (Int -> a) -> HaloState props context state action -> Effect a
fresh f (HaloState s) = Ref.modify' (\a -> { state: a + 1, value: f a }) s.fresh
