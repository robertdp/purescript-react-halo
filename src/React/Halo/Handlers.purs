module React.Halo.Handlers
  ( Handlers
  , defaultHandlers
  ) where

import Prelude

import React.Halo.Internal.Runtime (Handlers) as Runtime

-- | Activation, prop-change, and action callbacks for a Halo component.
-- |
-- | Each invocation is an independent root owned by the current React
-- | activation. `onActivate` is repeatable under StrictMode, not an exactly-once
-- | mount callback. `onPropsChange` receives the previous props; use
-- | `React.Halo.getProps` to read current props. `onAction` starts for every
-- | dispatched action, so action handlers can overlap.
-- |
-- | There is no asynchronous deactivation callback because React cleanup is
-- | synchronous. Use Aff finalizers for forked processes and emitter cleanup
-- | for subscriptions.
type Handlers props state action m = Runtime.Handlers props state action m

-- | Handlers that do nothing. Use a record update to configure only the
-- | callbacks a component needs.
defaultHandlers :: forall props state action m. Handlers props state action m
defaultHandlers =
  { onActivate: pure unit
  , onPropsChange: \_ -> pure unit
  , onAction: \_ -> pure unit
  }
