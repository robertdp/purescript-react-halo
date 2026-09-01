module React.Halo.Internal.Types
  ( Activity(..)
  , ErrorContext(..)
  , ForkId(..)
  , SubscriptionId(..)
  , TaskCounts
  , TaskPolicy(..)
  , activityFor
  , activityTotals
  , emptyActivity
  ) where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))

-- | Scheduling semantics for an explicit component-scoped task.
-- |
-- | `Every` starts every submitted task concurrently and is unkeyed.
-- | `Restartable key` cancels prior running work and discards its queue.
-- | `Drop key` ignores a submission while that key is busy. `Enqueue key` runs
-- | every submission FIFO, one at a time. `KeepLatest key` lets the current task
-- | finish while retaining only the newest queued submission.
data TaskPolicy key
  = Every
  | Restartable key
  | Drop key
  | Enqueue key
  | KeepLatest key

-- | Identifies the operation whose unexpected failure reached `onError`.
-- |
-- | `PropsChangeError` carries the previous props. `TaskError` carries the
-- | policy used when the explicit task was submitted.
data ErrorContext props action key
  = ActivationError
  | DeactivationError
  | PropsChangeError props
  | ActionError action
  | TaskError (TaskPolicy key)

-- | Counts of explicit scheduled tasks. Handler and structured-child execution
-- | is intentionally excluded.
type TaskCounts =
  { running :: Int
  , queued :: Int
  }

-- | A renderable snapshot of explicit task activity. Unkeyed `Every` tasks
-- | appear in totals but not under a key.
newtype Activity key = Activity
  { total :: TaskCounts
  , byKey :: Map key TaskCounts
  }

derive newtype instance eqActivity :: Eq key => Eq (Activity key)

derive newtype instance showActivity :: Show key => Show (Activity key)

emptyActivity :: forall key. Activity key
emptyActivity = Activity
  { total: { running: 0, queued: 0 }
  , byKey: Map.empty
  }

-- | Read total running and queued explicit task counts.
activityTotals :: forall key. Activity key -> TaskCounts
activityTotals (Activity activity) = activity.total

-- | Read explicit task counts for one key.
activityFor :: forall key. Ord key => key -> Activity key -> TaskCounts
activityFor key (Activity activity) =
  case Map.lookup key activity.byKey of
    Just counts -> counts
    Nothing -> { running: 0, queued: 0 }

-- | Identifies a component-scoped emitter subscription.
newtype SubscriptionId = SubscriptionId Int

derive newtype instance eqSubscriptionId :: Eq SubscriptionId

derive newtype instance ordSubscriptionId :: Ord SubscriptionId

derive newtype instance showSubscriptionId :: Show SubscriptionId

-- | Identifies a structured child created with `fork`.
newtype ForkId = ForkId Int

derive newtype instance eqForkId :: Eq ForkId

derive newtype instance ordForkId :: Ord ForkId

derive newtype instance showForkId :: Show ForkId
