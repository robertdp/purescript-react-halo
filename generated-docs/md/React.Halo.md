## Module React.Halo

#### `component`

``` purescript
component :: forall state action props. String -> ComponentSpec props state action Aff -> Effect (props -> JSX)
```

#### `component_`

``` purescript
component_ :: forall state action. String -> ComponentSpec Unit state action Aff -> Effect JSX
```

#### `UseHalo`

``` purescript
newtype UseHalo props state action hooks
```

##### Instances
``` purescript
Newtype (UseHalo props state action hooks) _
```

#### `useHalo`

``` purescript
useHalo :: forall state action props. HookSpec props state action Aff -> Hook (UseHalo props state action) (state /\ (action -> Effect Unit))
```


### Re-exported from Control.Monad.Error.Class:

#### `throwError`

``` purescript
throwError :: forall a m e. MonadThrow e m => e -> m a
```

### Re-exported from Control.Monad.Reader:

#### `ask`

``` purescript
ask :: forall m r. MonadAsk r m => m r
```

#### `asks`

``` purescript
asks :: forall r m a. MonadAsk r m => (r -> a) -> m a
```

Projects a value from the global context in a `MonadAsk`.

### Re-exported from Control.Monad.State.Class:

#### `state`

``` purescript
state :: forall a m s. MonadState s m => (s -> (Tuple a s)) -> m a
```

#### `put`

``` purescript
put :: forall m s. MonadState s m => s -> m Unit
```

Set the state.

#### `modify_`

``` purescript
modify_ :: forall s m. MonadState s m => (s -> s) -> m Unit
```

#### `modify`

``` purescript
modify :: forall s m. MonadState s m => (s -> s) -> m s
```

Modify the state by applying a function to the current state. The returned
value is the new state value.

#### `gets`

``` purescript
gets :: forall s m a. MonadState s m => (s -> a) -> m a
```

Get a value which depends on the current state.

#### `get`

``` purescript
get :: forall m s. MonadState s m => m s
```

Get the current state.

### Re-exported from Control.Monad.Trans.Class:

#### `lift`

``` purescript
lift :: forall m a t. MonadTrans t => Monad m => m a -> t m a
```

### Re-exported from Control.Monad.Writer:

#### `tell`

``` purescript
tell :: forall m w. MonadTell w m => w -> m Unit
```

### Re-exported from Effect.Aff.Class:

#### `liftAff`

``` purescript
liftAff :: forall m. MonadAff m => Aff ~> m
```

### Re-exported from Effect.Class:

#### `liftEffect`

``` purescript
liftEffect :: forall a m. MonadEffect m => Effect a -> m a
```

### Re-exported from React.Halo.Component:

#### `Lifecycle`

``` purescript
data Lifecycle props action
  = Initialize props
  | Update props props
  | Action action
  | Finalize
```

#### `HookSpec`

``` purescript
type HookSpec props state action m = { eval :: Lifecycle props action -> HaloM props state action m Unit, initialState :: state, props :: props }
```

#### `ComponentSpec`

``` purescript
type ComponentSpec props state action m = { eval :: Lifecycle props action -> HaloM props state action m Unit, initialState :: state, render :: { props :: props, send :: action -> Effect Unit, state :: state } -> JSX }
```

### Re-exported from React.Halo.Component.Control:

#### `SubscriptionId`

``` purescript
newtype SubscriptionId
```

##### Instances
``` purescript
Eq SubscriptionId
Ord SubscriptionId
```

#### `HaloM`

``` purescript
newtype HaloM props state action m a
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
```

##### Instances
``` purescript
Newtype (HaloAp props state action m a) _
Functor (HaloAp props state action m)
Apply (HaloAp props state action m)
Applicative (HaloAp props state action m)
```

#### `ForkId`

``` purescript
newtype ForkId
```

##### Instances
``` purescript
Eq ForkId
Ord ForkId
```

#### `unsubscribe`

``` purescript
unsubscribe :: forall m action state props. SubscriptionId -> HaloM props state action m Unit
```

#### `subscribe'`

``` purescript
subscribe' :: forall m action state props. (SubscriptionId -> Event action) -> HaloM props state action m SubscriptionId
```

#### `subscribe`

``` purescript
subscribe :: forall props state action m. Event action -> HaloM props state action m SubscriptionId
```

#### `props`

``` purescript
props :: forall props m action state. HaloM props state action m props
```

#### `kill`

``` purescript
kill :: forall m action state props. ForkId -> HaloM props state action m Unit
```

#### `hoist`

``` purescript
hoist :: forall props state action m m'. Functor m => (m ~> m') -> (HaloM props state action m) ~> (HaloM props state action m')
```

#### `fork`

``` purescript
fork :: forall m action state props. HaloM props state action m Unit -> HaloM props state action m ForkId
```

### Re-exported from React.Halo.Component.Eval:

#### `EvalSpec`

``` purescript
type EvalSpec props action state m = { onAction :: action -> HaloM props state action m Unit, onFinalize :: Maybe action, onInitialize :: props -> Maybe action, onUpdate :: props -> props -> Maybe action }
```

#### `makeEval`

``` purescript
makeEval :: forall props action state m. EvalSpec props action state m -> Lifecycle props action -> HaloM props state action m Unit
```

#### `defaultEval`

``` purescript
defaultEval :: forall props action state m. EvalSpec props action state m
```

