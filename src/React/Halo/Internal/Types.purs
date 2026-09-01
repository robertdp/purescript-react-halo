module React.Halo.Internal.Types
  ( Activity(..)
  , ErrorContext(..)
  , ForkId(..)
  , Lifecycle(..)
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

-- | Evaluations driven by the React component lifecycle and by dispatched actions.
-- |
-- | React may activate, deactivate, and reactivate the same hook instance (notably
-- | in development StrictMode), so `Activate` is repeatable. `Update` carries the
-- | previous props; current props are available from `Halo.props`.
data Lifecycle props action
  = Activate
  | Update props
  | Action action

-- | How a dispatched action is scheduled. Keyed policies coordinate actions that
-- | return the same user-defined key. `Every` is intentionally unkeyed.
data TaskPolicy key
  = Every
  | Restartable key
  | Drop key
  | Enqueue key
  | KeepLatest key

-- | The evaluation whose unexpected `Aff` failure reached the error handler.
data ErrorContext props action
  = ActivationError
  | DeactivationError
  | UpdateError props
  | ActionError action

-- | Running and queued work counts.
type TaskCounts =
  { running :: Int
  , queued :: Int
  }

-- | A renderable snapshot of scheduler activity. Unkeyed `Every` work appears
-- | in the totals but not under a key.
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

activityTotals :: forall key. Activity key -> TaskCounts
activityTotals (Activity activity) = activity.total

activityFor :: forall key. Ord key => key -> Activity key -> TaskCounts
activityFor key (Activity activity) =
  case Map.lookup key activity.byKey of
    Just counts -> counts
    Nothing -> { running: 0, queued: 0 }

newtype SubscriptionId = SubscriptionId Int

derive newtype instance eqSubscriptionId :: Eq SubscriptionId

derive newtype instance ordSubscriptionId :: Ord SubscriptionId

derive newtype instance showSubscriptionId :: Show SubscriptionId

newtype ForkId = ForkId Int

derive newtype instance eqForkId :: Eq ForkId

derive newtype instance ordForkId :: Ord ForkId

derive newtype instance showForkId :: Show ForkId
