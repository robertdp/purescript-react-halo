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
import Halogen.Subscription (Emitter)
import React.Halo.Internal.Types (ForkId, SubscriptionId)

-- | The Halo evaluation algebra
-- |
-- | - `props` are the component props
-- | - `ctx` is some component context
-- | - `state` is the component state
-- | - `action` is the set of actions that the component handles
-- | - `m` is the monad used during evaluation
-- | - `a` is the result type
data HaloF props ctx state action (m :: Type -> Type) a
  = Props (props -> a)
  | Context (ctx -> a)
  | State (state -> Tuple a state)
  | Subscribe (SubscriptionId -> Emitter action) (SubscriptionId -> a)
  | Unsubscribe SubscriptionId a
  | Lift (m a)
  | Par (HaloAp props ctx state action m a)
  | Fork (HaloM props ctx state action m Unit) (ForkId -> a)
  | Kill ForkId a

instance functorHaloF :: Functor m => Functor (HaloF props ctx state action m) where
  map f = case _ of
    Props k -> Props (f <<< k)
    Context k -> Context (f <<< k)
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
-- | - `ctx` is some component context
-- | - `state` is the component state
-- | - `action` is the set of actions that the component handles
-- | - `m` is the monad used during evaluation
-- | - `a` is the result type
newtype HaloM props ctx state action (m :: Type -> Type) a
  = HaloM (Free (HaloF props ctx state action m) a)

derive newtype instance functorHaloM :: Functor (HaloM props ctx state action m)

derive newtype instance applyHaloM :: Apply (HaloM props ctx state action m)

derive newtype instance applicativeHaloM :: Applicative (HaloM props ctx state action m)

derive newtype instance bindHaloM :: Bind (HaloM props ctx state action m)

derive newtype instance monadHaloM :: Monad (HaloM props ctx state action m)

derive newtype instance semigroupHaloM :: Semigroup a => Semigroup (HaloM props ctx state action m a)

derive newtype instance monoidHaloM :: Monoid a => Monoid (HaloM props ctx state action m a)

instance monadTransHaloM :: MonadTrans (HaloM props ctx state action) where
  lift = HaloM <<< liftF <<< Lift

instance monadEffectHaloM :: MonadEffect m => MonadEffect (HaloM props ctx state action m) where
  liftEffect = lift <<< liftEffect

instance monadAffHaloM :: MonadAff m => MonadAff (HaloM props ctx state action m) where
  liftAff = lift <<< liftAff

instance monadStateHaloM :: MonadState state (HaloM props ctx state action m) where
  state = HaloM <<< liftF <<< State

instance monadRecHaloM :: MonadRec (HaloM props ctx state action m) where
  tailRecM k a =
    k a
      >>= case _ of
          Loop x -> tailRecM k x
          Done y -> pure y

instance monadAskHaloM :: MonadAsk r m => MonadAsk r (HaloM props ctx state action m) where
  ask = lift ask

instance monadTellHaloM :: MonadTell w m => MonadTell w (HaloM props ctx state action m) where
  tell = lift <<< tell

instance monadThrowHaloM :: MonadThrow e m => MonadThrow e (HaloM props ctx state action m) where
  throwError = lift <<< throwError

-- | The Halo parallel evaluation applicative. It lifts `HaloM` into a free applicative.
-- |
-- | - `props` are the component props
-- | - `ctx` is some component context
-- | - `state` is the component state
-- | - `action` is the set of actions that the component handles
-- | - `m` is the monad used during evaluation
-- | - `a` is the result type
newtype HaloAp props ctx state action (m :: Type -> Type) a
  = HaloAp (FreeAp (HaloM props ctx state action m) a)

derive newtype instance functorHaloAp :: Functor (HaloAp props ctx state action m)

derive newtype instance applyHaloAp :: Apply (HaloAp props ctx state action m)

derive newtype instance applicativeHaloAp :: Applicative (HaloAp props ctx state action m)

instance parallelHaloM :: Parallel (HaloAp props ctx state action m) (HaloM props ctx state action m) where
  parallel = HaloAp <<< liftFreeAp
  sequential = HaloM <<< liftF <<< Par

-- | Hoist (transform) the base monad of a `HaloM` expression.
hoist :: forall props ctx state action m m'. Functor m => (m ~> m') -> HaloM props ctx state action m ~> HaloM props ctx state action m'
hoist nat (HaloM component) = HaloM (hoistFree go component)
  where
  go :: HaloF props ctx state action m ~> HaloF props ctx state action m'
  go = case _ of
    Props k -> Props k
    Context k -> Context k
    State k -> State k
    Subscribe event k -> Subscribe event k
    Unsubscribe sid a -> Unsubscribe sid a
    Lift m -> Lift (nat m)
    Par par -> Par (hoistAp nat par)
    Fork m k -> Fork (hoist nat m) k
    Kill fid a -> Kill fid a

-- | Hoist (transform) the base applicative of a `HaloAp` expression.
hoistAp :: forall props ctx state action m m'. Functor m => (m ~> m') -> HaloAp props ctx state action m ~> HaloAp props ctx state action m'
hoistAp nat (HaloAp component) = HaloAp (hoistFreeAp (hoist nat) component)

-- | Read the current props.
props :: forall props ctx state action m. HaloM props ctx state action m props
props = HaloM (liftF (Props identity))

-- | Read the current context.
context :: forall props ctx state action m. HaloM props ctx state action m ctx
context = HaloM (liftF (Context identity))

-- | Subscribe to new actions from an `Emitter`. Subscriptions will be automatically cancelled when the component
-- | unmounts.
-- |
-- | Returns a `SubscriptionId` which can be used with `unsubscribe` to manually cancel a subscription.
subscribe :: forall props ctx state action m. Emitter action -> HaloM props ctx state action m SubscriptionId
subscribe = subscribe' <<< const

-- | Same as `subscribe` but the event-producing logic is also passed the `SuscriptionId`. This is useful when events
-- | need to unsubscribe themselves.
subscribe' :: forall props ctx state action m. (SubscriptionId -> Emitter action) -> HaloM props ctx state action m SubscriptionId
subscribe' event = HaloM (liftF (Subscribe event identity))

-- | Cancels the event subscription belonging to the `SubscriptionId`.
unsubscribe :: forall props ctx state action m. SubscriptionId -> HaloM props ctx state action m Unit
unsubscribe sid = HaloM (liftF (Unsubscribe sid unit))

-- | Start a `HaloM` process running independantly from the current "thread". Forks are tracked automatically and
-- | killed when the `Finalize` event occurs (when the component unmounts). New forks can still be created during the
-- | `Finalize` event, but once evaluation ends there will be no way of killing them.
-- |
-- | Returns a `ForkId` for the new process.
fork :: forall props ctx state action m. HaloM props ctx state action m Unit -> HaloM props ctx state action m ForkId
fork m = HaloM (liftF (Fork m identity))

-- | Kills the process belonging to the `ForkId`.
kill :: forall props ctx state action m. ForkId -> HaloM props ctx state action m Unit
kill fid = HaloM (liftF (Kill fid unit))
