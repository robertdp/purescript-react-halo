## Module React.Halo.Internal.Types

#### `Lifecycle`

``` purescript
data Lifecycle props action
  = Initialize props
  | Update props props
  | Action action
  | Finalize
```

The Halo lifecycle events.

- `Initialize` contains the initial props. It occurs when the component mounts, and only once per component.
- `Update` contains the previous and new props. It occurs when the component re-renders and the props have changes.
- `Action` contains the dispatched action. It occurs each time an action is dispatched to be eval'd, up until the
  `Finalize` event
- `Finalize` occurs when the component unmounts.

#### `SubscriptionId`

``` purescript
newtype SubscriptionId
  = SubscriptionId Int
```

##### Instances
``` purescript
Eq SubscriptionId
Ord SubscriptionId
```

#### `ForkId`

``` purescript
newtype ForkId
  = ForkId Int
```

##### Instances
``` purescript
Eq ForkId
Ord ForkId
```


