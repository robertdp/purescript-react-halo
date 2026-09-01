# Halo v4 API reference

Import the intentional public surface from `React.Halo`:

```purescript
import React.Halo as Halo
```

Runtime constructors and ownership records are internal and not exported from this module.

## Core computation

```purescript
HaloM props state action key a
```

`HaloM` runs directly on `Aff` in a private scoped environment. It has `Functor`, `Apply`, `Applicative`, `Bind`, `Monad`, `MonadState state`, `MonadEffect`, and `MonadAff` instances.

Type parameters:

- `props`: current React component props;
- `state`: Halo-owned component state;
- `action`: events accepted by `dispatch` and subscriptions;
- `key`: application-defined explicit task keys; and
- `a`: computation result.

## Handlers

```purescript
type Handlers props state action key =
  { onActivate :: HaloM props state action key Unit
  , onAction :: action -> HaloM props state action key Unit
  , onPropsChange :: props -> HaloM props state action key Unit
  }

defaultHandlers :: forall props state action key. Handlers props state action key
```

`defaultHandlers` ignores all callbacks. Use PureScript record update syntax to replace selected fields.

- `onActivate` runs for every React effect activation and may run more than once for one hook instance.
- `onAction` starts for each action dispatched while the scope is active.
- `onPropsChange previousProps` starts when the props reference changes. Read current props with `props`.

Handlers are scope-owned, concurrent, and excluded from `Activity`.

## Task submission and cancellation

```purescript
startTask
  :: Ord key
  => TaskPolicy key
  -> HaloM props state action key Unit
  -> HaloM props state action key Unit

cancelTask
  :: Ord key
  => key
  -> HaloM props state action key Unit
```

`startTask` submits component-scoped work and returns without waiting for it. The task outlives successful completion of the submitting handler or task. It is cancelled on scope deactivation.

`cancelTask key` synchronously fences running tasks for `key`, discards the queue, requests cancellation, publishes new activity, and returns. It cannot target unkeyed `Every` tasks.

```purescript
data TaskPolicy key
  = Every
  | Restartable key
  | Drop key
  | Enqueue key
  | KeepLatest key
```

- `Every`: starts all submissions concurrently.
- `Restartable key`: replaces running and queued work for the key.
- `Drop key`: discards a submission while the key is busy.
- `Enqueue key`: runs every submission FIFO, one at a time.
- `KeepLatest key`: lets current work finish and retains only the newest queued submission.

## Activity

```purescript
type TaskCounts =
  { running :: Int
  , queued :: Int
  }

activityTotals :: Activity key -> TaskCounts
activityFor :: Ord key => key -> Activity key -> TaskCounts
emptyActivity :: Activity key
```

`Activity` counts explicit `startTask` submissions only. Totals include unkeyed and keyed tasks. `activityFor` reports one keyed slot. Handler execution and structured children are excluded.

## State and props

```purescript
props :: HaloM props state action key props
```

Use `MonadState` operations for state. `props` returns the latest component props. State mutation and capability acquisition are commit-fenced when the current owner becomes stale.

## Structured children

```purescript
fork
  :: HaloM props state action key Unit
  -> HaloM props state action key ForkId

kill
  :: ForkId
  -> HaloM props state action key Unit
```

`fork` creates a concurrent child owned by the current handler or task. Parent completion or cancellation cancels the child. `kill` requests earlier cancellation. `ForkId` is abstract from `React.Halo`.

## Subscriptions and emitters

```purescript
makeEmitter
  :: ((action -> Effect Unit) -> Effect (Effect Unit))
  -> Emitter action

subscribe
  :: Ord key
  => Emitter action
  -> HaloM props state action key SubscriptionId

subscribe'
  :: Ord key
  => (SubscriptionId -> Emitter action)
  -> HaloM props state action key SubscriptionId

unsubscribe
  :: SubscriptionId
  -> HaloM props state action key Unit
```

Emitter registration receives a receiver and returns its cleanup effect. Subscription emissions dispatch actions into the activation scope that registered them. Stale callbacks cannot target a later scope.

Manual unsubscription removes tracking before cleanup runs. Deactivation attempts all remaining cleanup effects; throwing cleanup is reported as `DeactivationError` without preventing other cleanup and cancellation requests.

`SubscriptionId` is abstract from `React.Halo`.

## Errors

```purescript
data ErrorContext props action key
  = ActivationError
  | DeactivationError
  | PropsChangeError props
  | ActionError action
  | TaskError (TaskPolicy key)
```

Every hook or component spec supplies:

```purescript
onError :: ErrorContext props action key -> Error -> Effect Unit
```

Halo sends unexpected handler and task failures to this callback. `DeactivationError` is reserved for throwing subscription cleanup. Halo suppresses cancellation errors it initiated.

## Hook API

```purescript
type HookSpec props state action key =
  { handlers :: Handlers props state action key
  , initialState :: state
  , onError :: ErrorContext props action key -> Error -> Effect Unit
  , props :: props
  }

type HaloHook state action key =
  { activity :: Activity key
  , dispatch :: action -> Effect Unit
  , state :: state
  }

useHalo
  :: Ord key
  => HookSpec props state action key
  -> Hook (UseHalo props state action key) (HaloHook state action key)
```

The hook synchronizes the latest handlers and callbacks on each React effect cycle. Activation cleanup deactivates the owned scope; StrictMode reactivation creates a fresh scope.

## Component API

```purescript
type ComponentSpec props state action key =
  { handlers :: Handlers props state action key
  , initialState :: props -> state
  , onError :: ErrorContext props action key -> Error -> Effect Unit
  , render ::
      { activity :: Activity key
      , dispatch :: action -> Effect Unit
      , props :: props
      , state :: state
      }
      -> JSX
  }

component
  :: Ord key
  => String
  -> ComponentSpec props state action key
  -> Component props
```

Use `component` when Halo owns the entire component. Use `useHalo` when other React hooks share the render function.
