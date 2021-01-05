## Module React.Halo


### Re-exported from Control.Monad.Error.Class:

#### `throwError`

``` purescript
throwError :: forall a m e. MonadThrow e m => e -> m a
```

### Re-exported from Control.Monad.Reader.Class:

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

### Re-exported from Control.Monad.Writer.Class:

#### `tell`

``` purescript
tell :: forall m w. MonadTell w m => w -> m Unit
```

### Re-exported from Control.Parallel.Class:

#### `parallel`

``` purescript
parallel :: forall m f. Parallel f m => m ~> f
```

#### `sequential`

``` purescript
sequential :: forall m f. Parallel f m => f ~> m
```

### Re-exported from Data.Tuple.Nested:

#### `(/\)`

``` purescript
infixr 6 Tuple as /\
```

Shorthand for constructing n-tuples as nested pairs.
`a /\ b /\ c /\ d /\ unit` becomes `Tuple a (Tuple b (Tuple c (Tuple d unit)))`

#### `type (/\)`

``` purescript
infixr 6 type Tuple as ype (/\
```

Shorthand for constructing n-tuple types as nested pairs.
`forall a b c d. a /\ b /\ c /\ d /\ Unit` becomes
`forall a b c d. Tuple a (Tuple b (Tuple c (Tuple d Unit)))`

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

### Re-exported from React.Basic.Hooks:

#### `JSX`

``` purescript
data JSX :: Type
```

Represents rendered React VDOM (the result of calling `React.createElement`
in JavaScript).

`JSX` is a `Monoid`:

- `append`
  - Merge two `JSX` nodes using `React.Fragment`.
- `mempty`
  - The `empty` node; renders nothing.

__*Hint:* Many useful utility functions already exist for Monoids. For example,
  `guard` can be used to conditionally render a subtree of components.__

##### Instances
``` purescript
Semigroup JSX
Monoid JSX
```

### Re-exported from React.Halo.Component:

#### `UseHalo`

``` purescript
newtype UseHalo props state action hooks
```

##### Instances
``` purescript
Newtype (UseHalo props state action hooks) _
```

#### `HookSpec`

``` purescript
type HookSpec props state action m = { eval :: Lifecycle props action -> HaloM props state action m Unit, initialState :: state, props :: props }
```

#### `ComponentSpec`

``` purescript
type ComponentSpec props state action m = { eval :: Lifecycle props action -> HaloM props state action m Unit, initialState :: state, render :: { props :: props, send :: action -> Effect Unit, state :: state } -> JSX }
```

#### `useHalo`

``` purescript
useHalo :: forall state action props. HookSpec props state action Aff -> Hook (UseHalo props state action) (state /\ (action -> Effect Unit))
```

Run renderless Halo in the current component. This allows Halo to be used with other hooks and other ways of
building components.

#### `component_`

``` purescript
component_ :: forall state action. String -> ComponentSpec Unit state action Aff -> Effect JSX
```

Build a propless component by providing a name and Halo component spec.

#### `component`

``` purescript
component :: forall state action props. String -> ComponentSpec props state action Aff -> Effect (props -> JSX)
```

Build a component by providing a name and Halo component spec.

### Re-exported from React.Halo.Internal.Control:

#### `HaloM`

``` purescript
newtype HaloM props state action m a
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
```

The Halo parallel evaluation applicative. It lifts `HaloM` into a free applicative.

- `props` are the component props
- `state` is the component state
- `action` is the set of actions that the component handles
- `m` is the monad used during evaluation
- `a` is the result type

##### Instances
``` purescript
Functor (HaloAp props state action m)
Apply (HaloAp props state action m)
Applicative (HaloAp props state action m)
Parallel (HaloAp props state action m) (HaloM props state action m)
```

#### `unsubscribe`

``` purescript
unsubscribe :: forall m action state props. SubscriptionId -> HaloM props state action m Unit
```

Cancels the event subscription belonging to the `SubscriptionId`.

#### `subscribe'`

``` purescript
subscribe' :: forall m action state props. (SubscriptionId -> Event action) -> HaloM props state action m SubscriptionId
```

Same as `subscribe` but the event-producing logic is also passed the `SuscriptionId`. This is useful when events
need to unsubscribe themselves.

#### `subscribe`

``` purescript
subscribe :: forall props state action m. Event action -> HaloM props state action m SubscriptionId
```

Subscribe to new actions from an `Event`. Subscriptions will be automatically cancelled when the component
unmounts.

Returns a `SubscriptionId` which can be used with `unsubscribe` to manually cancel a subscription.

#### `props`

``` purescript
props :: forall props m action state. HaloM props state action m props
```

Read the current props.

#### `kill`

``` purescript
kill :: forall m action state props. ForkId -> HaloM props state action m Unit
```

Kills the process belonging to the `ForkId`.

#### `hoist`

``` purescript
hoist :: forall props state action m m'. Functor m => (m ~> m') -> (HaloM props state action m) ~> (HaloM props state action m')
```

Hoist (transform) the base monad of a `HaloM` expression.

#### `fork`

``` purescript
fork :: forall m action state props. HaloM props state action m Unit -> HaloM props state action m ForkId
```

Start a `HaloM` process running independantly from the current "thread". Forks are tracked automatically and
killed when the `Finalize` event occurs (when the component unmounts). New forks can still be created during the
`Finalize` event, but once evaluation ends there will be no way of killing them.

Returns a `ForkId` for the new process.

### Re-exported from React.Halo.Internal.Eval:

#### `EvalSpec`

``` purescript
type EvalSpec props state action m = { onAction :: action -> HaloM props state action m Unit, onFinalize :: Maybe action, onInitialize :: props -> Maybe action, onUpdate :: props -> props -> Maybe action }
```

A simpler interface for building the components eval function. The main lifecycle events map directly into
actions, so only the action handling logic needs to be written using `HaloM`.

#### `makeEval`

``` purescript
makeEval :: forall m action state props. (EvalSpec props state action m -> EvalSpec props state action m) -> Lifecycle props action -> HaloM props state action m Unit
```

Given an `EvalSpec` builder, it will return an eval function.

#### `defaultEval`

``` purescript
defaultEval :: forall props action state m. EvalSpec props state action m
```

The empty `EvalSpec`.

### Re-exported from React.Halo.Internal.Types:

#### `SubscriptionId`

``` purescript
newtype SubscriptionId
```

##### Instances
``` purescript
Eq SubscriptionId
Ord SubscriptionId
```

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

#### `ForkId`

``` purescript
newtype ForkId
```

##### Instances
``` purescript
Eq ForkId
Ord ForkId
```

