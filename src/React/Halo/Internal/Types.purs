module React.Halo.Internal.Types where

import Prelude

data Lifecycle props action
  = Initialize props
  | Update props props
  | Action action
  | Finalize

newtype SubscriptionId
  = SubscriptionId Int

derive newtype instance eqSubscriptionId :: Eq SubscriptionId

derive newtype instance ordSubscriptionId :: Ord SubscriptionId

newtype ForkId
  = ForkId Int

derive newtype instance eqForkId :: Eq ForkId

derive newtype instance ordForkId :: Ord ForkId
