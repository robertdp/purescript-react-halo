module React.Halo.Handlers
  ( Handlers
  , defaultHandlers
  ) where

import Prelude

import React.Halo.Internal.Runtime (Handlers) as Runtime

-- | Activation, prop-change, and action callbacks for a Halo component.
-- |
-- | `onActivate` may run again after React replays an effect setup. It is not an
-- | exactly-once mount callback. `onPropsChange` receives the previous props;
-- | use `React.Halo.getProps` to read the current props. `onAction` starts for
-- | every dispatched action.
type Handlers props state action m = Runtime.Handlers props state action m

-- | Handlers that do nothing. Use a record update to configure only the
-- | callbacks a component needs.
defaultHandlers :: forall props state action m. Handlers props state action m
defaultHandlers =
  { onActivate: pure unit
  , onPropsChange: \_ -> pure unit
  , onAction: \_ -> pure unit
  }
