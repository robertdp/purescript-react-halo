-- | Component-scoped action handling for PureScript React.
-- |
-- | A Halo component keeps application effects in a caller-defined monad `m`.
-- | `component` or `useHalo` receives a natural transformation from `m` to
-- | `Aff`, while `HaloM` adds component props, state, actions, cancellable
-- | forks, synchronous cleanup, subscriptions, and lifecycle ownership.
-- |
-- | Import this module for the intentional public API. Runtime constructors and
-- | ownership records remain internal.
module React.Halo
  ( module Exports
  ) where

import React.Halo.Component (ComponentSpec, component) as Exports
import React.Halo.Handlers (Handlers, defaultHandlers) as Exports
import React.Halo.Hook (HaloResult, HookSpec, UseHalo, useHalo) as Exports
import React.Halo.Internal.Runtime (HaloAp, HaloM, fork, getProps, kill, registerCleanup, releaseCleanup, subscribe, subscribeWithId, unsubscribe) as Exports
import React.Halo.Internal.Types (CleanupId, ErrorContext(..), ForkId, SubscriptionId) as Exports
import React.Halo.Subscription (Emitter, makeEmitter) as Exports
