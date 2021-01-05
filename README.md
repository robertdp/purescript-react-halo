# React Halo

Halo is a [Halogen](https://github.com/purescript-halogen/purescript-halogen)-inspired interface for React.

It is available as a hook: `useHalo`; for building entire components there is: `component` and `component_`.

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-react-halo).

## Using with [Spago](https://github.com/purescript/spago)

Update the additions in your `packages.dhall`:

```dhall
let additions =
  { react-halo =
    { dependencies = [ "aff", "free", "freeap", "react-basic-hooks", "wire" ]
    , repo = "https://github.com/robertdp/purescript-react-halo.git"
    , version = "v1.0.0"
    }
  , wire =
    { dependencies = [ "aff", "filterable", "refs", "unsafe-reference" ]
    , repo = "https://github.com/robertdp/purescript-wire.git"
    , version = "v0.5.0"
    }
  }
```

Then install with Spago:

`$ spago install react-halo`

## What does Halo provide?

Whether you are using the hook or one of the component helpers, the main feature that Halo provides is the `eval` function. It looks like:

```purescript
Lifecycle props action -> HaloM props state action m a
```

where `Lifecycle` is:

```purescript
data Lifecycle props action
  = Initialize props    -- when the component mounts
  | Update props props  -- when the props change
  | Action action       -- when an action is dispatched
  | Finalize            -- when the component unmounts
```

`HaloM` is also a monad transformer, and so you can lift any monad `m`  logic into `HaloM`. Just be aware that in order to run the logic, Halo requires that you `hoist` (convert) your chosen monad into `Aff` before returning it.

### Hoisting

```purescript
hoist :: forall props state action m m'. Functor m => (m ~> m') -> HaloM props state action m ~> HaloM props state action m'
```

Example:

```purescript
invertReaderT x = ReaderT \env -> Halo.hoist (flip runReaderT env) x
```

### Working with props

```purescript
props :: forall props action state m. HaloM props state action m props
```

Example:

```purescript
fireOnChange value = do
  { onChange } <- Halo.props
  onChange value
```

### Working with state

`HaloM` doesn't have any special interface for reading and modifying state, instead providing an instance of [MonadState](https://pursuit.purescript.org/packages/purescript-transformers/docs/Control.Monad.State.Class) for flexibility.

### Subscriptions

Subscriptions registered using these functions are automatically tracked by Halo.

```purescript
subscribe :: forall props state action m. Event action -> HaloM props state action m SubscriptionId

unsubscribe :: forall m action state props. SubscriptionId -> HaloM props state action m Unit
```

`Event` comes from the [Wire](https://github.com/robertdp/purescript-wire) library.

There is also a version for subscriptions that want to unsubscribe themselves:

```purescript
subscribe' :: forall m action state props. (SubscriptionId -> Event action) -> HaloM props state action m SubscriptionId
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
