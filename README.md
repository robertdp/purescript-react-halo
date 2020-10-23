# React Halo

Halo is a [Halogen](https://github.com/purescript-halogen/purescript-halogen)-inspired interface and runtime for your components, designed to integrate with React.

It is implemented as a hook: `useHalo`; and simple component helpers are included: `component` and `component_`.

## Using with [Spago](https://github.com/purescript/spago)

Update the additions in your `packages.dhall`:

```dhall
let additions =
  { react-halo =
    { dependencies = [ "aff", "free", "freeap", "react-basic-hooks", "wire" ]
    , repo = "https://github.com/robertdp/purescript-react-halo.git"
    , version = "v0.2.1"
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

## Generated documentation

See [here](https://github.com/robertdp/purescript-react-halo/blob/master/generated-docs/md/React.Halo.md)

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

## Hoisting

```purescript
hoist :: forall props state action m m'. Functor m => (m ~> m') -> HaloM props state action m ~> HaloM props state action m'
```

Example:

```purescript
invertReaderT x = ReaderT \env -> Halo.hoist (flip runReaderT env) x
```

## Working with props

```purescript
props :: forall props action state m. HaloM props state action m props
```

Example:

```purescript
fireOnChange value = do
  { onChange } <- Halo.props
  onChange value
```

## Working with state

`HaloM` doesn't have any special interface for reading and modifying state, instead providing an instance of [MonadState](https://pursuit.purescript.org/packages/purescript-transformers/docs/Control.Monad.State.Class) for flexibility.

## Subscriptions

`HaloM` also provides functions for subscriptions management:

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

## Parallelism

And finally, `HaloM` provides functions for creating and killing forks which run in parallel (or as useful an approximation as we can get in JavaScript):

```purescript
fork :: forall m action state props. HaloM props state action m Unit -> HaloM props state action m ForkId

kill :: forall m action state props. ForkId -> HaloM props state action m Unit
```

Similarly to subscriptions, when the component unmounts all still-running forks will be killed. However new forks _can_ be created during the `Finalize` phase but there is no way of killing them (as with Halogen).
