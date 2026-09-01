module React.Halo.Internal.Control
  ( HaloM
  , fork
  , kill
  , props
  , subscribe
  , subscribe'
  , unsubscribe
  ) where

import Prelude

import Halogen.Subscription (Emitter)
import React.Halo.Internal.Runtime as Runtime
import React.Halo.Internal.Types (ForkId, SubscriptionId)

type HaloM props state action key = Runtime.HaloM props state action key

props :: forall props state action key. HaloM props state action key props
props = Runtime.props

subscribe
  :: forall props state action key
   . Ord key
  => Emitter action
  -> HaloM props state action key SubscriptionId
subscribe = Runtime.subscribe

subscribe'
  :: forall props state action key
   . Ord key
  => (SubscriptionId -> Emitter action)
  -> HaloM props state action key SubscriptionId
subscribe' = Runtime.subscribe'

unsubscribe
  :: forall props state action key
   . SubscriptionId
  -> HaloM props state action key Unit
unsubscribe = Runtime.unsubscribe

fork
  :: forall props state action key
   . HaloM props state action key Unit
  -> HaloM props state action key ForkId
fork = Runtime.fork

kill
  :: forall props state action key
   . ForkId
  -> HaloM props state action key Unit
kill = Runtime.kill
