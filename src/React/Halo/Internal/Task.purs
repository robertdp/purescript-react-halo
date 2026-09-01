module React.Halo.Internal.Task
  ( debounce
  , debounceWith
  , once
  , reset
  , startIfInactive
  , supersede
  ) where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as Aff
import React.Halo.Internal.Runtime (HaloM, managedComplete, managedReset, managedStart)
import React.Halo.Internal.Task.Types (Slot)
import React.Halo.Internal.Task.Types as Task

data Policy
  = Once
  | IfInactive
  | Supersede

-- | Start only from `Idle`. Typed failure and success remain terminal until
-- | `reset`.
once
  :: forall name props componentState action m error result
   . Slot name componentState error result
  -> HaloM props componentState action m (Either error result)
  -> HaloM props componentState action m Unit
once = launch Once Nothing

-- | Start from `Idle`, `Failed`, or `Succeeded`, but preserve active work.
startIfInactive
  :: forall name props componentState action m error result
   . Slot name componentState error result
  -> HaloM props componentState action m (Either error result)
  -> HaloM props componentState action m Unit
startIfInactive = launch IfInactive Nothing

-- | Make a new invocation authoritative immediately, fencing and requesting
-- | cancellation of prior managed work without waiting for its finalizers.
supersede
  :: forall name props componentState action m error result
   . Slot name componentState error result
  -> HaloM props componentState action m (Either error result)
  -> HaloM props componentState action m Unit
supersede = launch Supersede Nothing

-- | Trailing-edge latest-wins task invocation. The private cancellable timer and
-- | the executing body both project to `Active`.
debounce
  :: forall name props componentState action m error result
   . Slot name componentState error result
  -> Milliseconds
  -> HaloM props componentState action m (Either error result)
  -> HaloM props componentState action m Unit
debounce = debounceWith Aff.delay

-- Internal deterministic timer seam used by runtime tests.
debounceWith
  :: forall name props componentState action m error result
   . (Milliseconds -> Aff Unit)
  -> Slot name componentState error result
  -> Milliseconds
  -> HaloM props componentState action m (Either error result)
  -> HaloM props componentState action m Unit
debounceWith schedule target duration =
  launch Supersede (Just (schedule (nonNegative duration))) target

-- | Clear terminal state or cancel authoritative active work. Cancellation is
-- | fenced synchronously and waits for Aff finalizers before returning.
reset
  :: forall name props componentState action m error result
   . Slot name componentState error result
  -> HaloM props componentState action m Unit
reset target = managedReset (Task.bindingOf target) \_ _ authority componentState ->
  case Task.statusAt target authority componentState of
    Task.Idle -> Nothing
    Task.Active -> Just
      { cancel: authority
      , state: Task.idleSlot target componentState
      }
    Task.Failed _ -> Just
      { cancel: Nothing
      , state: Task.idleSlot target componentState
      }
    Task.Succeeded _ -> Just
      { cancel: Nothing
      , state: Task.idleSlot target componentState
      }

launch
  :: forall name props componentState action m error result
   . Policy
  -> Maybe (Aff Unit)
  -> Slot name componentState error result
  -> HaloM props componentState action m (Either error result)
  -> HaloM props componentState action m Unit
launch policy privateDelay target body =
  managedStart (Task.bindingOf target) privateDelay \runtimeId generation forkId authority componentState -> do
    let
      status = Task.statusAt target authority componentState
      canStart = case policy of
        Once -> case status of
          Task.Idle -> true
          _ -> false
        IfInactive -> case status of
          Task.Active -> false
          _ -> true
        Supersede -> true
    if canStart then do
      let
        token = Task.makeToken target runtimeId generation forkId
        taskBody = do
          outcome <- body
          managedComplete (Task.bindingOf target) token
            (Task.completeSlot target token outcome)
      pure
        { cancel: case policy of
            Supersede -> authority
            _ -> Nothing
        , computation: taskBody
        , state: Task.activateSlot target token componentState
        , token
        }
    else Nothing

nonNegative :: Milliseconds -> Milliseconds
nonNegative (Milliseconds duration) = Milliseconds (max 0.0 duration)
