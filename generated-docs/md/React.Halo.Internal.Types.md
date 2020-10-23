## Module React.Halo.Internal.Types

#### `Lifecycle`

``` purescript
data Lifecycle props action
  = Initialize props
  | Update props props
  | Action action
  | Finalize
```

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


