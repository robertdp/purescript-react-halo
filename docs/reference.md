# Halo v4 API reference

Import the intentional public surface from `React.Halo`:

```purescript
import React.Halo as Halo
```

Runtime constructors and ownership records are internal. `ForkId` and `SubscriptionId` constructors are hidden.

## Core computation

```purescript
HaloM props state action m a
HaloAp props state action m a
```

`HaloM` is the sequential component computation. Its parameters are current props, Halo state, dispatched actions, the application's base monad, and the result.

It has unconditional `Functor`, `Apply`, `Applicative`, `Bind`, `Monad`, and `MonadState state` instances. It is a `MonadTrans` in its `m` parameter:

```purescript
lift :: Monad m => m a -> HaloM props state action m a
```

The following capabilities are lifted through `m`, rather than executed directly in Halo's private `Aff`:

```purescript
MonadEffect m => MonadEffect (HaloM props state action m)
MonadAff m => MonadAff (HaloM props state action m)
MonadAsk environment m => MonadAsk environment (HaloM props state action m)
MonadTell output m => MonadTell output (HaloM props state action m)
MonadThrow error m => MonadThrow error (HaloM props state action m)
```

Each `lift` checks the root fence before invoking the captured interpreter. A stale root therefore cannot start a new application effect, even when it catches its initial Aff cancellation.

`HaloAp` is the abstract parallel applicative counterpart:

```purescript
Parallel
  (HaloAp props state action m)
  (HaloM props state action m)
```

Parallel branches share their root, component scope, and `m ~> Aff` interpreter snapshot. Concurrent Halo state writes have nondeterministic ordering; combine independent results before committing state when possible.

## Handlers

```purescript
type Handlers props state action m =
  { onActivate :: HaloM props state action m Unit
  , onPropsChange :: props -> HaloM props state action m Unit
  , onAction :: action -> HaloM props state action m Unit
  }

defaultHandlers
  :: forall props state action m
   . Handlers props state action m
```

Handlers are component-activation-owned roots:

- `onActivate` runs for each React effect activation and is repeatable under StrictMode.
- `onPropsChange previousProps` starts when the props reference changes.
- `onAction action` starts for each dispatch while active.

Handlers selected for new roots come from the latest hook spec. There is no asynchronous deactivation handler.

## State and props

Use the `MonadState state` operations for Halo state.

```purescript
getProps
  :: forall props state action m
   . HaloM props state action m props
```

`getProps` returns the latest props. State commits from a stale root are ignored.

## Component-owned forks

```purescript
fork
  :: forall props state action m
   . HaloM props state action m Unit
  -> HaloM props state action m ForkId

kill
  :: forall props state action m
   . ForkId
  -> HaloM props state action m Unit
```

`fork` starts a root owned by the current React activation. It may outlive its launching handler. The child uses the launching root's interpreter snapshot, but receives an independent state/capability fence and reports unexpected failures as `ForkError id`.

`kill` removes a tracked fork, fences its Halo state and capabilities synchronously, and then waits for Aff cancellation and finalizers. Killing an unknown or completed ID does nothing. Deactivation fences and requests cancellation of every remaining fork and handler.

## Subscriptions and emitters

```purescript
makeEmitter
  :: forall action
   . ((action -> Effect Unit) -> Effect (Effect Unit))
  -> Emitter action

subscribe
  :: forall props state action m
   . Emitter action
  -> HaloM props state action m SubscriptionId

subscribeWithId
  :: forall props state action m
   . (SubscriptionId -> Emitter action)
  -> HaloM props state action m SubscriptionId

unsubscribe
  :: forall props state action m
   . SubscriptionId
  -> HaloM props state action m Unit
```

Emitter registration receives a receiver and returns its cleanup effect. Emissions dispatch into the activation that registered the receiver. A stale callback cannot target a later activation.

Manual unsubscription removes tracking before cleanup runs. Deactivation attempts every remaining cleanup; throwing cleanup is isolated and reported as `DeactivationError` without preventing other cleanup and cancellation requests.

## Errors

```purescript
data ErrorContext props action
  = ActivationError
  | PropsChangeError props
  | ActionError action
  | ForkError ForkId
  | DeactivationError

onError
  :: ErrorContext props action
  -> Error
  -> Effect Unit
```

Halo reports unexpected root failures through the latest `onError` callback. Cancellation initiated by Halo is suppressed. Expected domain failures should be represented in application values, actions, or state.

## Hook API

```purescript
type HookSpec props state action m =
  { handlers :: Handlers props state action m
  , initialState :: state
  , onError :: ErrorContext props action -> Error -> Effect Unit
  , props :: props
  }

type HaloResult state action =
  { dispatch :: action -> Effect Unit
  , state :: state
  }

useHalo
  :: forall props state action m
   . (m ~> Aff)
  -> HookSpec props state action m
  -> Hook
       (UseHalo props state action m)
       (HaloResult state action)
```

Each new handler captures the latest natural transformation supplied to the hook. A fork inherits the transformation captured by the root that launches it, even if a newer render supplied another interpreter before the fork starts. Cleanup deactivates the current scope; StrictMode reactivation creates a fresh usable scope.

## Component API

```purescript
type ComponentSpec props state action m =
  { handlers :: Handlers props state action m
  , initialState :: props -> state
  , onError :: ErrorContext props action -> Error -> Effect Unit
  , render ::
      { dispatch :: action -> Effect Unit
      , props :: props
      , state :: state
      }
      -> JSX
  }

component
  :: forall props state action m
   . String
  -> (m ~> Aff)
  -> ComponentSpec props state action m
  -> Component props
```

`initialState` receives initial props once per mount. Later prop changes invoke `handlers.onPropsChange` and do not reinitialize state. Use `component` when Halo owns the whole component and `useHalo` when other hooks share the render function.
