module React.Halo.Internal.Types
  ( ErrorContext(..)
  , ForkId(..)
  , SubscriptionId(..)
  ) where

import Prelude

-- | Identifies the component-owned computation whose unexpected failure reached
-- | `onError`.
data ErrorContext props action
  = ActivationError
  | PropsChangeError props
  | ActionError action
  | ForkError ForkId
  | DeactivationError

-- | Identifies a component-owned fiber created with `fork`.
newtype ForkId = ForkId Int

derive newtype instance eqForkId :: Eq ForkId

derive newtype instance ordForkId :: Ord ForkId

derive newtype instance showForkId :: Show ForkId

-- | Identifies a component-scoped emitter subscription.
newtype SubscriptionId = SubscriptionId Int

derive newtype instance eqSubscriptionId :: Eq SubscriptionId

derive newtype instance ordSubscriptionId :: Ord SubscriptionId

derive newtype instance showSubscriptionId :: Show SubscriptionId
