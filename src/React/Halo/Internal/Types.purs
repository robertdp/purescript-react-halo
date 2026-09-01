module React.Halo.Internal.Types
  ( CleanupId(..)
  , ErrorContext(..)
  , ForkId(..)
  , SubscriptionId(..)
  ) where

import Prelude

-- | Identifies synchronous cleanup in one React activation. Its constructor is
-- | hidden from the root `React.Halo` API.
newtype CleanupId = CleanupId Int

derive newtype instance eqCleanupId :: Eq CleanupId

derive newtype instance ordCleanupId :: Ord CleanupId

derive newtype instance showCleanupId :: Show CleanupId

-- | Identifies the component-owned computation or cleanup whose unexpected
-- | failure reached `onError`.
-- |
-- | `PropsChangeError` carries the previous props, `ActionError` carries the
-- | dispatched action, and `ForkError` carries the component-owned fork ID.
-- | Halo-initiated cancellation is fenced and is not reported.
data ErrorContext props action
  = ActivationError
  | PropsChangeError props
  | ActionError action
  | ForkError ForkId
  | DeactivationError

-- | Identifies a component-owned process created with `fork`. Its constructor
-- | is hidden from the root `React.Halo` API.
newtype ForkId = ForkId Int

derive newtype instance eqForkId :: Eq ForkId

derive newtype instance ordForkId :: Ord ForkId

derive newtype instance showForkId :: Show ForkId

-- | Identifies an emitter subscription in one React activation. Its constructor
-- | is hidden from the root `React.Halo` API.
newtype SubscriptionId = SubscriptionId Int

derive newtype instance eqSubscriptionId :: Eq SubscriptionId

derive newtype instance ordSubscriptionId :: Ord SubscriptionId

derive newtype instance showSubscriptionId :: Show SubscriptionId
