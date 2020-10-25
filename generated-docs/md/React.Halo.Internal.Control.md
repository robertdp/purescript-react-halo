## Module React.Halo.Internal.Control

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

The Halo evaluation algebra

- `props` are the component props
- `state` is the component state
- `action` is the set of actions that the component handles
- `m` is the monad used during evaluation
- `a` is the result type

##### Instances
``` purescript
(Functor m) => Functor (HaloF props state action m)
```

#### `HaloM`

``` purescript
newtype HaloM props state action m a
  = HaloM (Free (HaloF props state action m) a)
```

The Halo evaluation monad. It lifts the `HaloF` algebra into a free monad.

- `props` are the component props
- `state` is the component state
- `action` is the set of actions that the component handles
- `m` is the monad used during evaluation
- `a` is the result type

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
Parallel (HaloAp props state action m) (HaloM props state action m)
```

#### `HaloAp`

``` purescript
newtype HaloAp props state action m a
  = HaloAp (FreeAp (HaloM props state action m) a)
```

The Halo parallel evaluation applicative. It lifts `HaloM` into a free applicative.

- `props` are the component props
- `state` is the component state
- `action` is the set of actions that the component handles
- `m` is the monad used during evaluation
- `a` is the result type

##### Instances
``` purescript
Newtype (HaloAp props state action m a) _
Functor (HaloAp props state action m)
Apply (HaloAp props state action m)
Applicative (HaloAp props state action m)
Parallel (HaloAp props state action m) (HaloM props state action m)
```

#### `hoist`

``` purescript
hoist :: forall props state action m m'. Functor m => (m ~> m') -> (HaloM props state action m) ~> (HaloM props state action m')
```

Hoist (transform) the base monad of a `HaloM` expression.

#### `props`

``` purescript
props :: forall props m action state. HaloM props state action m props
```

Read the current props.

#### `subscribe`

``` purescript
subscribe :: forall props state action m. Event action -> HaloM props state action m SubscriptionId
```

Subscribe to new actions from an `Event`. Subscriptions will be automatically cancelled when the component
unmounts.

Returns a `SubscriptionId` which can be used with `unsubscribe` to manually cancel a subscription.

#### `subscribe'`

``` purescript
subscribe' :: forall m action state props. (SubscriptionId -> Event action) -> HaloM props state action m SubscriptionId
```

Same as `subscribe` but the event-producing logic is also passed the `SuscriptionId`. This is useful when events
need to unsubscribe themselves.

#### `unsubscribe`

``` purescript
unsubscribe :: forall m action state props. SubscriptionId -> HaloM props state action m Unit
```

Cancels the event subscription belonging to the `SubscriptionId`.

#### `fork`

``` purescript
fork :: forall m action state props. HaloM props state action m Unit -> HaloM props state action m ForkId
```

Start a `HaloM` process running independantly from the current "thread". Forks are tracked automatically and
killed when the `Finalize` event occurs (when the component unmounts). New forks can still be created during the
`Finalize` event, but once evaluation ends there will be no way of killing them.

Returns a `ForkId` for the new process.

#### `kill`

``` purescript
kill :: forall m action state props. ForkId -> HaloM props state action m Unit
```

Kills the process belonging to the `ForkId`.


