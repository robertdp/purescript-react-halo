## Module React.Halo.Component.Control

#### `HaloF`

``` purescript
data HaloF props state action m a
  = Props (props -> a)
  | State (state -> Tuple a state)
  | Subscribe (SubscriptionId -> Event action) (SubscriptionId -> a)
  | Unsubscribe SubscriptionId a
  | Lift (m a)
  | Par (HaloAp props state action m a)
  | Fork (HaloM props state action m Unit) (ForkId -> a)
  | Kill ForkId a
```

##### Instances
``` purescript
(Functor m) => Functor (HaloF props state action m)
```

#### `HaloM`

``` purescript
newtype HaloM props state action m a
  = HaloM (Free (HaloF props state action m) a)
```

##### Instances
``` purescript
Functor (HaloM props state action m)
Apply (HaloM props state action m)
Applicative (HaloM props state action m)
Bind (HaloM props state action m)
Monad (HaloM props state action m)
(Semigroup a) => Semigroup (HaloM props state action m a)
(Monoid a) => Monoid (HaloM props state action m a)
MonadTrans (HaloM props state action)
(MonadEffect m) => MonadEffect (HaloM props state action m)
(MonadAff m) => MonadAff (HaloM props state action m)
MonadState state (HaloM props state action m)
MonadRec (HaloM props state action m)
(MonadAsk r m) => MonadAsk r (HaloM props state action m)
(MonadTell w m) => MonadTell w (HaloM props state action m)
(MonadThrow e m) => MonadThrow e (HaloM props state action m)
```

#### `HaloAp`

``` purescript
newtype HaloAp props state action m a
  = HaloAp (FreeAp (HaloM props state action m) a)
```

##### Instances
``` purescript
Newtype (HaloAp props state action m a) _
Functor (HaloAp props state action m)
Apply (HaloAp props state action m)
Applicative (HaloAp props state action m)
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

#### `hoist`

``` purescript
hoist :: forall props state action m m'. Functor m => (m ~> m') -> (HaloM props state action m) ~> (HaloM props state action m')
```

#### `props`

``` purescript
props :: forall props m action state. HaloM props state action m props
```

#### `subscribe'`

``` purescript
subscribe' :: forall m action state props. (SubscriptionId -> Event action) -> HaloM props state action m SubscriptionId
```

#### `subscribe`

``` purescript
subscribe :: forall props state action m. Event action -> HaloM props state action m SubscriptionId
```

#### `unsubscribe`

``` purescript
unsubscribe :: forall m action state props. SubscriptionId -> HaloM props state action m Unit
```

#### `fork`

``` purescript
fork :: forall m action state props. HaloM props state action m Unit -> HaloM props state action m ForkId
```

#### `kill`

``` purescript
kill :: forall m action state props. ForkId -> HaloM props state action m Unit
```


