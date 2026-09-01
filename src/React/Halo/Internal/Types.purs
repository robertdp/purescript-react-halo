module React.Halo.Internal.Types
  ( Activity(..)
  , ErrorContext(..)
  , ForkId(..)
  , SubscriptionId(..)
  , TaskCounts
  , activityAtKey
  , activityTotals
  , emptyActivity
  ) where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))

-- | Identifies the operation whose unexpected failure reached `onError`.
-- |
-- | `PropsChangeError` carries the previous props. Task failures and task
-- | configuration conflicts carry the affected task key.
data ErrorContext props action key
  = ActivationError
  | DeactivationError
  | PropsChangeError props
  | ActionError action
  | TaskError key
  | TaskConfigurationError key

-- | Counts of explicit scheduled tasks. Handler and structured-child execution
-- | is intentionally excluded.
type TaskCounts =
  { running :: Int
  , queued :: Int
  }

-- | A renderable snapshot of explicit task activity. Every task is keyed, so
-- | the total is the sum of the per-key counts.
newtype Activity key = Activity
  { total :: TaskCounts
  , byKey :: Map key TaskCounts
  }

derive newtype instance eqActivity :: Eq key => Eq (Activity key)

derive newtype instance showActivity :: Show key => Show (Activity key)

-- | An activity snapshot with no running or queued tasks.
emptyActivity :: forall key. Activity key
emptyActivity = Activity
  { total: { running: 0, queued: 0 }
  , byKey: Map.empty
  }

-- | Read total running and queued explicit task counts.
activityTotals :: forall key. Activity key -> TaskCounts
activityTotals (Activity activity) = activity.total

-- | Internal keyed lookup used by the abstract Task API.
activityAtKey :: forall key. Ord key => key -> Activity key -> TaskCounts
activityAtKey key (Activity activity) =
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
