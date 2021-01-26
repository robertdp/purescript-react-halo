module React.Halo.Internal.Control where

import Prelude
import Control.Applicative.Free (FreeAp, hoistFreeAp, liftFreeAp)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Free (Free, hoistFree, liftF)
import Control.Monad.Reader (class MonadAsk, ask)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.State (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer (class MonadTell, tell)
import Control.Parallel (class Parallel)
import Data.Bifunctor (lmap)
import Data.Tuple (Tuple)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import FRP.Event (Event)
import React.Halo.Internal.Types (ForkId, SubscriptionId)

-- | The Halo evaluation algebra
-- |
-- | - `props` are the component props
-- | - `state` is the component state
-- | - `action` is the set of actions that the component handles
-- | - `m` is the monad used during evaluation
-- | - `a` is the result type
data HaloF props state action m a
  = Props (props -> a)
  | State (state -> Tuple a state)
  | Subscribe (SubscriptionId -> Event action) (SubscriptionId -> a)
  | Unsubscribe SubscriptionId a
  | Lift (m a)
  | Par (HaloAp props state action m a)
  | Fork (HaloM props state action m Unit) (ForkId -> a)
  | Kill ForkId a

instance functorHaloF :: Functor m => Functor (HaloF props state action m) where
  map f = case _ of
    Props k -> Props (f <<< k)
    State k -> State (lmap f <<< k)
    Subscribe fes k -> Subscribe fes (map f k)
    Unsubscribe sid a -> Unsubscribe sid (f a)
    Lift m -> Lift (map f m)
    Par par -> Par (map f par)
    Fork m k -> Fork m (map f k)
    Kill fid a -> Kill fid (f a)

-- | The Halo evaluation monad. It lifts the `HaloF` algebra into a free monad.
-- |
-- | - `props` are the component props
-- | - `state` is the component state
-- | - `action` is the set of actions that the component handles
-- | - `m` is the monad used during evaluation
-- | - `a` is the result type
newtype HaloM props state action m a
  = HaloM (Free (HaloF props state action m) a)

derive newtype instance functorHaloM :: Functor (HaloM props state action m)

derive newtype instance applyHaloM :: Apply (HaloM props state action m)

derive newtype instance applicativeHaloM :: Applicative (HaloM props state action m)

derive newtype instance bindHaloM :: Bind (HaloM props state action m)

derive newtype instance monadHaloM :: Monad (HaloM props state action m)

derive newtype instance semigroupHaloM :: Semigroup a => Semigroup (HaloM props state action m a)

derive newtype instance monoidHaloM :: Monoid a => Monoid (HaloM props state action m a)

instance monadTransHaloM :: MonadTrans (HaloM props state action) where
  lift = HaloM <<< liftF <<< Lift

instance monadEffectHaloM :: MonadEffect m => MonadEffect (HaloM props state action m) where
  liftEffect = lift <<< liftEffect

instance monadAffHaloM :: MonadAff m => MonadAff (HaloM props state action m) where
  liftAff = lift <<< liftAff

instance monadStateHaloM :: MonadState state (HaloM props state action m) where
  state = HaloM <<< liftF <<< State

instance monadRecHaloM :: MonadRec (HaloM props state action m) where
  tailRecM k a =
    k a
      >>= case _ of
          Loop x -> tailRecM k x
          Done y -> pure y

instance monadAskHaloM :: MonadAsk r m => MonadAsk r (HaloM props state action m) where
  ask = lift ask

instance monadTellHaloM :: MonadTell w m => MonadTell w (HaloM props state action m) where
  tell = lift <<< tell

instance monadThrowHaloM :: MonadThrow e m => MonadThrow e (HaloM props state action m) where
  throwError = lift <<< throwError

-- | The Halo parallel evaluation applicative. It lifts `HaloM` into a free applicative.
-- |
-- | - `props` are the component props
-- | - `state` is the component state
-- | - `action` is the set of actions that the component handles
-- | - `m` is the monad used during evaluation
-- | - `a` is the result type
newtype HaloAp props state action m a
  = HaloAp (FreeAp (HaloM props state action m) a)

derive newtype instance functorHaloAp :: Functor (HaloAp props state action m)

derive newtype instance applyHaloAp :: Apply (HaloAp props state action m)

derive newtype instance applicativeHaloAp :: Applicative (HaloAp props state action m)

instance parallelHaloM :: Parallel (HaloAp props state action m) (HaloM props state action m) where
  parallel = HaloAp <<< liftFreeAp
  sequential = HaloM <<< liftF <<< Par

-- | Hoist (transform) the base monad of a `HaloM` expression.
hoist :: forall props state action m m'. Functor m => (m ~> m') -> HaloM props state action m ~> HaloM props state action m'
hoist nat (HaloM component) = HaloM (hoistFree go component)
  where
  go :: HaloF props state action m ~> HaloF props state action m'
  go = case _ of
    Props k -> Props k
    State k -> State k
    Subscribe event k -> Subscribe event k
    Unsubscribe sid a -> Unsubscribe sid a
    Lift m -> Lift (nat m)
    Par par -> Par (hoistAp nat par)
    Fork m k -> Fork (hoist nat m) k
    Kill fid a -> Kill fid a

-- | Hoist (transform) the base applicative of a `HaloAp` expression.
hoistAp :: forall props state action m m'. Functor m => (m ~> m') -> HaloAp props state action m ~> HaloAp props state action m'
hoistAp nat (HaloAp component) = HaloAp (hoistFreeAp (hoist nat) component)

-- | Read the current props.
props :: forall props m action state. HaloM props state action m props
props = HaloM (liftF (Props identity))

-- | Subscribe to new actions from an `Event`. Subscriptions will be automatically cancelled when the component
-- | unmounts.
-- |
-- | Returns a `SubscriptionId` which can be used with `unsubscribe` to manually cancel a subscription.
subscribe :: forall props state action m. Event action -> HaloM props state action m SubscriptionId
subscribe = subscribe' <<< const

-- | Same as `subscribe` but the event-producing logic is also passed the `SuscriptionId`. This is useful when events
-- | need to unsubscribe themselves.
subscribe' :: forall m action state props. (SubscriptionId -> Event action) -> HaloM props state action m SubscriptionId
subscribe' event = HaloM (liftF (Subscribe event identity))

-- | Cancels the event subscription belonging to the `SubscriptionId`.
unsubscribe :: forall m action state props. SubscriptionId -> HaloM props state action m Unit
unsubscribe sid = HaloM (liftF (Unsubscribe sid unit))

-- | Start a `HaloM` process running independantly from the current "thread". Forks are tracked automatically and
-- | killed when the `Finalize` event occurs (when the component unmounts). New forks can still be created during the
-- | `Finalize` event, but once evaluation ends there will be no way of killing them.
-- |
-- | Returns a `ForkId` for the new process.
fork :: forall m action state props. HaloM props state action m Unit -> HaloM props state action m ForkId
fork m = HaloM (liftF (Fork m identity))

-- | Kills the process belonging to the `ForkId`.
kill :: forall m action state props. ForkId -> HaloM props state action m Unit
kill fid = HaloM (liftF (Kill fid unit))
