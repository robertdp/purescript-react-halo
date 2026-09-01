-- | Typed lifecycle state for component-owned Halo work.
-- |
-- | Import this module qualified. A `State error result` is stored inside
-- | component state and located with a standard lens; it is not a computation,
-- | key, or cache definition. Policy bodies remain ordinary `HaloM` values that
-- | return `Either error result`.
-- |
-- | The mutable representation is abstract because active state carries hidden
-- | run ownership. Render through `toStatus`, `asStatus`, `toMaybe`, or
-- | `isActive`. Expected `Left` values become `Failed`; unexpected exceptions
-- | return the task to `Idle` and follow Halo's normal `ForkError` routing.
module React.Halo.Task
  ( module Exports
  ) where

import React.Halo.Internal.Task
  ( State
  , Status(..)
  , _Active
  , _Failed
  , _Idle
  , _Succeeded
  , asStatus
  , debounce
  , idle
  , isActive
  , once
  , reset
  , startIfInactive
  , supersede
  , toMaybe
  , toStatus
  ) as Exports
