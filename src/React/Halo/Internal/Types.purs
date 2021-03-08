module React.Halo.Internal.Types where

import Prelude
import Data.Tuple (Tuple)

-- | The Halo lifecycle events.
-- |
-- | - `Initialize` contains the initial props. It occurs when the component mounts, and only once per component.
-- | - `Update` contains the previous and new props. It occurs when the component re-renders and the props have changes.
-- | - `Action` contains the dispatched action. It occurs each time an action is dispatched to be eval'd, up until the
-- |   `Finalize` event
-- | - `Finalize` occurs when the component unmounts.
data Lifecycle props context action
  = Initialize props context
  | Update (Tuple props props) (Tuple context context)
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
