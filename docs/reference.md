# Halo v4 API reference

Import the intentional public surface from `React.Halo`:

```purescript
import React.Halo as Halo
```

Runtime constructors, task representation, and scheduling strategies are internal.

## Core computation

```purescript
HaloM props state action key a
```

`HaloM` runs on `Aff` in a private scoped environment. It has `Functor`, `Apply`, `Applicative`, `Bind`, `Monad`, `MonadState state`, `MonadEffect`, and `MonadAff` instances.

The parameters are component props, Halo state, dispatched actions, application task keys, and the result. Task input is generic on each `Task`; it is deliberately not another `HaloM` parameter.

## Handlers

```purescript
type Handlers props state action key =
  { onActivate :: HaloM props state action key Unit
  , onAction :: action -> HaloM props state action key Unit
  , onPropsChange :: props -> HaloM props state action key Unit
  }

defaultHandlers :: forall props state action key. Handlers props state action key
```

`defaultHandlers` ignores every callback. Handlers are active-scope-owned, concurrent, commit-fenced, and excluded from task activity.

- `onActivate` runs for every React effect activation.
- `onAction` starts for each action dispatched while active.
- `onPropsChange previousProps` starts when the props reference changes; use `props` for current props.

## Task definitions

```purescript
Task props state action key input
```

`Task` is abstract. It binds a key, a strategy, and an `input -> HaloM ... Unit` implementation.

```purescript
concurrent
  :: key
  -> (input -> HaloM props state action key Unit)
  -> Task props state action key input

restartable
  :: key
  -> (input -> HaloM props state action key Unit)
  -> Task props state action key input

drop
  :: key
  -> (input -> HaloM props state action key Unit)
  -> Task props state action key input

enqueue
  :: key
  -> (input -> HaloM props state action key Unit)
  -> Task props state action key input

keepLatest
  :: key
  -> (input -> HaloM props state action key Unit)
  -> Task props state action key input
```

- `concurrent`: every performance starts immediately, including same-key work.
- `restartable`: fences/cancels running work, discards queued work, then starts the new input.
- `drop`: discards the new input while the key is busy.
- `enqueue`: preserves every input FIFO and runs one at a time.
- `keepLatest`: lets running work finish and retains only the newest queued input.

The first performed definition for a key fixes that key's strategy for the component runtime lifetime. Same key plus same strategy shares a slot. Same key plus a different strategy is rejected through `onError` as `TaskConfigurationError key`, including across deactivate/reactivate.

## Performance and cancellation

```purescript
perform
  :: Ord key
  => Task props state action key input
  -> input
  -> HaloM props state action key Unit

perform_
  :: Ord key
  => Task props state action key Unit
  -> HaloM props state action key Unit

cancel
  :: Ord key
  => Task props state action key input
  -> HaloM props state action key Unit
```

`perform` and `perform_` submit component-scoped work and return without waiting. The submitted work outlives successful completion of its caller and is cancelled on scope deactivation.

`cancel` synchronously fences running work and discards queued work for the task's key, requests cancellation, updates activity, and returns. All definitions sharing the key share this boundary.

## Activity

```purescript
type TaskCounts =
  { running :: Int
  , queued :: Int
  }

activity
  :: Ord key
  => Task props state action key input
  -> Activity key
  -> TaskCounts

activityTotals :: Activity key -> TaskCounts
emptyActivity :: Activity key
```

`activity task snapshot` reports the task key's slot; same-key definitions report the same counts. `activityTotals` sums every slot. Activity counts only `perform`/`perform_` submissions, not handlers, structured children, or subscriptions.

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

`fork` creates a concurrent child owned by the current handler or performed task. Parent completion or cancellation cancels the child. `kill` requests earlier cancellation. `ForkId` is abstract.

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

Emitter registration receives a receiver and returns cleanup. Emissions dispatch actions into the registering activation scope; stale callbacks cannot target a later scope. Manual unsubscribe removes tracking before cleanup. Deactivation attempts all remaining cleanup, and reports thrown cleanup as `DeactivationError` without preventing other cleanup and cancellation.

## Errors

```purescript
data ErrorContext props action key
  = ActivationError
  | DeactivationError
  | PropsChangeError props
  | ActionError action
  | TaskError key
  | TaskConfigurationError key

onError :: ErrorContext props action key -> Error -> Effect Unit
```

Unexpected handler and task failures reach `onError`. `TaskError` identifies the task key. `TaskConfigurationError` identifies a same-key strategy conflict. `DeactivationError` reports throwing subscription cleanup. Halo suppresses cancellation errors it initiated.

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

The hook synchronizes the latest handlers and React callbacks. Cleanup deactivates its scope; StrictMode reactivation creates a fresh scope while retaining the runtime's key-strategy validation.

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

Use `component` when Halo owns the whole component. Use `useHalo` when other React hooks share the render function.
