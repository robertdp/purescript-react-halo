module React.Halo.Task
  ( Task
  , activity
  , cancel
  , concurrent
  , drop
  , enqueue
  , keepLatest
  , perform
  , perform_
  , restartable
  ) where

import Prelude

import React.Halo.Internal.Runtime (HaloM)
import React.Halo.Internal.Runtime as Runtime
import React.Halo.Internal.Task as Internal
import React.Halo.Internal.Types (Activity, TaskCounts, activityAtKey)

-- | A reusable task definition. A task binds a user-defined key, one scheduling
-- | strategy, and an input-driven Halo computation. Its constructor is hidden;
-- | create tasks with `concurrent`, `restartable`, `drop`, `enqueue`, or
-- | `keepLatest`.
newtype Task props state action key input = Task
  (Internal.Task (HaloM props state action key) key input)

-- | Define a task whose performances for this key all run concurrently.
concurrent
  :: forall props state action key input
   . key
  -> (input -> HaloM props state action key Unit)
  -> Task props state action key input
concurrent key implementation = Task (Internal.Task key Internal.Concurrent implementation)

-- | Define a task whose newest performance cancels and replaces running and
-- | queued work for this key.
restartable
  :: forall props state action key input
   . key
  -> (input -> HaloM props state action key Unit)
  -> Task props state action key input
restartable key implementation = Task (Internal.Task key Internal.Restartable implementation)

-- | Define a task that ignores a performance while this key is busy.
drop
  :: forall props state action key input
   . key
  -> (input -> HaloM props state action key Unit)
  -> Task props state action key input
drop key implementation = Task (Internal.Task key Internal.Drop implementation)

-- | Define a task that runs every performance for this key FIFO, one at a time.
enqueue
  :: forall props state action key input
   . key
  -> (input -> HaloM props state action key Unit)
  -> Task props state action key input
enqueue key implementation = Task (Internal.Task key Internal.Enqueue implementation)

-- | Define a task that lets current work finish while retaining only the newest
-- | queued performance for this key.
keepLatest
  :: forall props state action key input
   . key
  -> (input -> HaloM props state action key Unit)
  -> Task props state action key input
keepLatest key implementation = Task (Internal.Task key Internal.KeepLatest implementation)

-- | Submit one task input and return immediately. The resulting work belongs to
-- | the active component scope, not to the handler or task that submitted it.
perform
  :: forall props state action key input
   . Ord key
  => Task props state action key input
  -> input
  -> HaloM props state action key Unit
perform (Task task) = Runtime.performTask task

-- | Submit a task whose input is `Unit`.
perform_
  :: forall props state action key
   . Ord key
  => Task props state action key Unit
  -> HaloM props state action key Unit
perform_ task = perform task unit

-- | Fence and cancel all running work and discard all queued work for the task's
-- | key. Definitions that intentionally share the key share this cancellation
-- | boundary.
cancel
  :: forall props state action key input
   . Ord key
  => Task props state action key input
  -> HaloM props state action key Unit
cancel (Task task) = Runtime.cancelDefinition task

-- | Read running and queued activity for the task's key. Definitions that share
-- | the key report the same counts.
activity
  :: forall props state action key input
   . Ord key
  => Task props state action key input
  -> Activity key
  -> TaskCounts
activity (Task task) = activityAtKey (Internal.key task)
