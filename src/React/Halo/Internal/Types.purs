module React.Halo.Internal.Types where

import Prelude

-- | The Halo lifecycle events.
-- |
-- | - `Initialize` contains the initial props. It occurs when the component mounts, and only once per component.
-- | - `Update` contains the previous and new props. It occurs when the component re-renders and the props have changes.
-- | - `Action` contains the dispatched action. It occurs each time an action is dispatched to be eval'd, up until the
-- |   `Finalize` event
-- | - `Finalize` occurs when the component unmounts.
data Lifecycle context action
  = Initialize context
  | Update context context
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
