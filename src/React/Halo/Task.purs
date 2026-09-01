-- | Typed lifecycle state for component-owned Halo work.
-- |
-- | Import this module qualified. A branded `Slot` is an identity-bearing
-- | optic for one `State error result` field; it is not a computation, input,
-- | fork handle, or cache definition. Policy bodies remain ordinary `HaloM`
-- | values that return `Either error result`.
-- |
-- | `State` is freely copyable, so active authority is validated through the
-- | immutable `View` published with each Halo render. Observe a slot with
-- | `toStatus`, `toMaybe`, or `isActive`. Expected `Left` values become
-- | `Failed`; unexpected exceptions return the task to `Idle` and follow Halo's
-- | normal `ForkError` routing.
module React.Halo.Task
  ( module Policies
  , module Types
  ) where

import React.Halo.Internal.Task (debounce, once, reset, startIfInactive, supersede) as Policies
import React.Halo.Internal.Task.Types (Slot, State, Status(..), View, _Active, _Failed, _Idle, _Succeeded, idle, isActive, slot, toMaybe, toStatus) as Types
