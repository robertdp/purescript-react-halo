# React Halo

Halo is a [Halogen](https://github.com/purescript-halogen/purescript-halogen)-inspired interface for React.

It is available as a hook: `useHalo`; for building entire components there is `component`.

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-react-halo).

## Using with [Spago](https://github.com/purescript/spago)

`$ spago install react-halo`
or
`$ npx spago install react-halo`

## What does Halo provide?

Whether you are using the hook or one of the component helpers, the main feature that Halo provides is the `eval` function. It looks like:

```purescript
Lifecycle props action -> HaloM props state action m Unit
```

where `Lifecycle` is:

```purescript
data Lifecycle props action
  = Initialize props    -- when the component mounts
  | Update props props  -- when the props change
  | Action action       -- when an action is dispatched
  | Finalize            -- when the component unmounts
```

The helper `mkEval` exists to make this easier to work with:

```purescript
data Action
  = Init
  | ...

onAction :: forall props state m. Action -> HaloM props state Action m Unit

eval = Halo.mkEval Halo.defaultEval { onInitialize = \props -> Just Init, onAction = onAction }
```

`HaloM` is also a monad transformer, and so you can lift any monad `m`  logic into `HaloM`. Just be aware that in order to run the logic, Halo requires that you `hoist` (convert) your chosen monad into `Aff` before returning it.

### Hoisting

```purescript
hoist :: forall props state action m m'. Functor m => (m ~> m') -> HaloM props state action m ~> HaloM props state action m'
```

Example:

```purescript
-- Inverting a reader
hoistReaderT ::
  forall props state action env m.
  HaloM props state action (ReaderT env m) ~>
  ReaderT env (HaloM props state action m)
hoistReaderT x = do
  env <- ask
  lift (Halo.hoist (flip runReaderT env) x)
```

### Working with props

```purescript
props :: forall props action state m. HaloM props state action m props
```

Example:

```purescript
fireOnChange ::
  forall props state action m a.
  MonadEffect m =>
  HaloM { onChange :: a -> Effect Unit | props } { value :: a | state } action m Unit
fireOnChange = do
  { onChange } <- Halo.props
  { value } <- Halo.get
  liftEffect (onChange value)
```

### Working with state

`HaloM` doesn't have any special interface for reading and modifying state, instead providing an instance of [MonadState](https://pursuit.purescript.org/packages/purescript-transformers/docs/Control.Monad.State.Class) for flexibility.

### Subscriptions

Subscriptions registered using these functions are automatically tracked by Halo.

```purescript
subscribe :: forall props state action m. Emitter action -> HaloM props state action m SubscriptionId

unsubscribe :: forall props state action m. SubscriptionId -> HaloM props state action m Unit
```

`Emitter` is from the `purescript-halogen-subscriptions` library.

There is also a version for subscriptions that want to unsubscribe themselves:

```purescript
subscribe' :: forall props state action m. (SubscriptionId -> Emitter action) -> HaloM props state action m SubscriptionId
```

Any subscriptions that remain when the component is unmounted are automatically unsubscribed. This prevents requiring manual clean up in the `Finalize` lifecycle event. Also note that new subscriptions will not be created once the `Finalize` event has been fired.

### Forking

Also provided are functions for creating and killing forks which launch processes in separate "threads" (or as useful an approximation as we can get in JavaScript):

```purescript
fork :: forall m action state props. HaloM props state action m Unit -> HaloM props state action m ForkId

kill :: forall m action state props. ForkId -> HaloM props state action m Unit
```

Similarly to subscriptions, when the component unmounts all still-running forks will be killed. However new forks _can_ be created during the `Finalize` phase but there is no way of killing them (as with Halogen).


### Parallelism

Finally `HaloM` provides an instance of `Parallel` for converting back and forth between `HaloAp`, it's applicative counterpart. This allows any logic to be easily converted to run in `parallel` or `sequential`ly.
