# Halo guide

Halo combines a typed UI action handler with component state and a runtime boundary for your application monad. Start with the [README quick start](../README.md), then use this guide when choosing lifetimes, cancellation, or cleanup behavior.

## Run application logic through `AppM`

An application monad commonly carries services or configuration over `Aff`:

```purescript
newtype AppM a = AppM (ReaderT AppEnv Aff a)

runAppM :: AppEnv -> AppM ~> Aff
runAppM env (AppM program) = runReaderT program env
```

Use it as the fourth `HaloM` parameter:

```purescript
type UI a = HaloM Props State Action AppM a
```

Standard transformer `lift` runs application logic inside a Halo computation:

```purescript
import Control.Monad.Trans.Class (lift)

loadAccount :: UI Unit
loadAccount = do
  account <- lift Account.load
  modify_ _ { account = Just account }
```

Supply the interpreter at the React boundary:

```purescript
Halo.component "Account" (runAppM env) spec
Halo.useHalo (runAppM env) hookSpec
```

Each new handler captures the latest interpreter supplied to the hook. A fork inherits the interpreter captured by the root that launches it. This keeps one running action on one application environment even when a later render supplies another interpreter.

The interpreter must return the `Aff` that performs the work. Do not detach it with `launchAff_`; Halo can own and cancel only the returned computation.

## Handle a UI action ADT

Rendering code receives `dispatch :: action -> Effect Unit`. Each dispatch starts `handlers.onAction action` in the current component scope:

```purescript
data Action
  = NameChanged String
  | Save
  | CancelSave

handlers = Halo.defaultHandlers
  { onAction = case _ of
      NameChanged name -> modify_ _ { name = name }
      Save -> save
      CancelSave -> cancelSave
  }
```

Action handlers overlap. A long-running action does not block a later dispatch, and React deactivation cancels every handler still running in that activation.

When a process must outlive its handler or another action must cancel it, start a component-owned fork instead of leaving the work in the handler.

## Update state without stale snapshots

`HaloM` has `MonadState state`. Use normal `get`, `put`, `gets`, `modify`, and `modify_` operations.

State operations run against the state current at that operation. Avoid reading a whole state value, waiting for an application effect, and then writing a modified copy of the old value:

```purescript
-- Avoid: another action can update state while save runs.
old <- get
result <- lift (save old.form)
put (old { result = Just result })
```

Capture only the input needed by the effect, then update the current state after it completes:

```purescript
form <- gets _.form
result <- lift (save form)
modify_ _ { result = Just result }
```

`Halo.getProps` reads the latest props. `onPropsChange` receives the previous props, so both sides of a synchronization are available:

```purescript
onPropsChange = \previous -> do
  current <- Halo.getProps
  synchronize previous current
```

Capture props before asynchronous work when that work must use one render's value. Otherwise, a later `getProps` intentionally returns newer props.

## Store typed task outcomes in component state

Import `React.Halo.Task` qualified when component state should retain the lifecycle and typed result of owned work. `Task.State error result` is abstract because it includes hidden cancellation identity. Locate it with a standard lens:

```purescript
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import React.Halo.Task as Task
import Type.Proxy (Proxy(..))

type State =
  { search :: Task.State SearchError Results
  , query :: String
  }

searchLens :: Lens' State (Task.State SearchError Results)
searchLens = prop (Proxy :: Proxy "search")

initialState =
  { search: Task.idle
  , query: ""
  }
```

A policy body remains ordinary `HaloM` and returns `Either error result`. It may update other component state. Halo atomically stores a matching `Left` as `Failed` or `Right` as `Succeeded`:

```purescript
Search query -> Task.supersede searchLens do
  modify_ _ { query = query }
  lift (Search.run query)

CancelSearch -> Task.reset searchLens
```

Choose a policy by invocation semantics:

- `once lens body` starts only from `Idle`; success and typed failure remain terminal until `reset`.
- `startIfInactive lens body` ignores a call while active, but starts from `Idle`, `Failed`, or `Succeeded`.
- `supersede lens body` makes every new call authoritative immediately. Prior work is fenced and cancellation is requested without waiting, so its finalizers may overlap the new body but cannot commit Halo state or begin another lifted application effect.
- `debounce lens milliseconds body` is trailing-edge latest-wins. Its private timer and body both render as `Active`; a new call cancels either phase. Nonpositive durations use a scheduled zero delay.
- `reset lens` publishes `Idle`, cancels active work, and waits for its Aff finalizers. Terminal state is cleared immediately.

Render through the read-only projection:

```purescript
case Task.toStatus state.search of
  Task.Idle -> renderPrompt
  Task.Active -> renderSpinner
  Task.Failed error -> renderError error
  Task.Succeeded results -> renderResults results
```

`Task.asStatus` is a standard read-only getter, and `_Idle`, `_Active`, `_Failed`, and `_Succeeded` are lawful prisms over `Task.Status`. `Task.toMaybe` returns only a succeeded result; `Task.isActive` covers both the private debounce timer and the executing body.

Expected failures belong in `Either`. An unexpected exception returns the matching task to `Idle` and is reported through the latest `onError` as `ForkError`. Cancellation is neither a typed failure nor an unexpected error. Put retry policy in AppM and lift the already-retrying computation; when nested under `debounce`, the debounce timer runs once and AppM then owns its attempts. A retry loop must let Aff cancellation propagate rather than catching every exception.

Task state is component-owned result storage, not a global cache. Calls do not retain an input or computation for later reruns.

## Start and kill component processes

`Halo.fork child` starts a process owned by the current React activation and returns a `ForkId`. The process may outlive the handler that created it:

```purescript
startSearch query = do
  previous <- gets _.searchFiber
  traverse_ Halo.kill previous

  fiber <- Halo.fork do
    modify_ _ { loading = true }
    results <- lift (Search.run query)
    modify_ _ { loading = false, results = results }

  modify_ _ { searchFiber = Just fiber }
```

`Halo.kill id` removes a tracked fork, fences it synchronously, requests Aff cancellation, and waits for cancellation and Aff finalizers before returning. Killing an unknown or completed ID does nothing.

A killed or deactivated root cannot commit Halo state, register another Halo-owned capability, or start a newly lifted application effect—even if it catches the initial Aff cancellation. Cancellation cannot retract an HTTP request, storage write, callback, or log that already happened. Design external writes for retry and idempotency where needed.

## Clean up at the correct boundary

Halo intentionally has no asynchronous `onDeactivate` handler. React effect cleanup is synchronous, so React cannot wait for a `HaloM`, `AppM`, or `Aff` callback. Starting detached work during cleanup would also escape component ownership.

Choose cleanup according to the resource:

- **Component process:** acquire and use the resource inside `fork` with an Aff finalizer. Deactivation requests cancellation of the fork.
- **Event source:** return synchronous cleanup from `makeEmitter`; Halo runs it while deactivating the subscription scope.
- **Other synchronous resource:** call `registerCleanup cleanup`. Call `releaseCleanup id` to remove and run it early.
- **User cancellation:** retain the `ForkId` and call `kill`, which waits for finalizers.
- **Persistence:** save during normal application flow. Do not rely on unmount completing asynchronous persistence.

`registerCleanup` accepts only `Effect Unit`, not `HaloM`, AppM, or `Aff`. `releaseCleanup` removes tracking before invoking the effect, so a throw is reported in the current root's error context and is not retried. Unknown and already released IDs are ignored.

Deactivation first fences the activation, then attempts every generic cleanup and subscription cleanup before requesting cancellation of handlers, forks, and tasks. A cleanup throw is reported as `DeactivationError` through the latest `onError` without blocking the other resources. No ordering between generic and subscription cleanup is part of the API. React cannot wait for Aff cancellation, but finalizers cannot commit Halo state or begin new lifted effects after the fence.

## Run independent work in parallel

`HaloM` has a direct `Parallel` instance with abstract counterpart `HaloAp`. Parallel branches share one root, component scope, and interpreter snapshot:

```purescript
loadDashboard = do
  Tuple profile feed <- sequential ado
    profile <- parallel (lift Profile.load)
    feed <- parallel (lift Feed.load)
    in Tuple profile feed

  modify_ _ { profile = profile, feed = feed }
```

Prefer independent application reads followed by one Halo state update. Concurrent state writes have nondeterministic ordering and can overwrite one another.

Parallel work is lexical: the combined computation waits for every branch. Use `fork` when work must continue independently of the launching handler or needs explicit cancellation by ID.

## Configure lifecycle handlers

```purescript
type Handlers props state action m =
  { onActivate :: HaloM props state action m Unit
  , onPropsChange :: props -> HaloM props state action m Unit
  , onAction :: action -> HaloM props state action m Unit
  }
```

Start with `defaultHandlers` and replace only the callbacks the component needs.

### `onActivate`

Halo calls this for every React effect activation. Development StrictMode can run setup, cleanup, then setup again for one hook instance. Treat activation as repeatable, not as an exactly-once mount event.

### `onPropsChange previousProps`

Halo starts this when the props reference changes. Read current props with `getProps`. The root uses the latest handlers and interpreter supplied to the hook.

### `onAction action`

Halo starts one root for each action dispatched while active, including actions emitted by subscriptions. Dispatch while inactive is ignored.

## Subscribe to event sources

Halo's emitter API avoids a Halogen dependency:

```purescript
events = Halo.makeEmitter \emit -> do
  listener <- source.listen emit
  pure (source.remove listener)
```

`subscribe events` registers an action source in the current activation. `subscribeWithId (\id -> emitterFor id)` also exposes the allocated `SubscriptionId`. `unsubscribe id` removes tracking before running cleanup.

Deactivation attempts every tracked cleanup even when one throws. Cleanup failures are reported as `DeactivationError` without preventing the remaining cleanup and cancellation requests. A callback retained by a faulty source stays bound to its original activation and cannot dispatch into a later StrictMode activation.

Emitters broadcast actions. They do not provide backpressure, consuming queues, or scheduling policies.

## Handle unexpected errors

Every component or hook spec supplies:

```purescript
onError :: ErrorContext props action -> Error -> Effect Unit
```

Contexts identify the failed root or cleanup:

- `ActivationError` for `onActivate`;
- `PropsChangeError previousProps` for `onPropsChange`;
- `ActionError action` for `onAction`;
- `ForkError id` for a component-owned fork; and
- `DeactivationError` for throwing subscription cleanup.

Halo selects the latest `onError` callback when reporting a failure. Expected domain failures belong in application values, actions, or state. Halo-initiated cancellation is suppressed after its root is fenced.

## Choose `component` or `useHalo`

Use `Halo.component` when Halo owns the complete component. Its renderer receives `{ props, state, dispatch }`. `initialState` receives initial props once per mount; synchronize later prop changes in `onPropsChange`.

Use `Halo.useHalo` when other React hooks share the render function. It accepts the same application interpreter and returns `{ state, dispatch }`.

## Common mistakes

- **An activation runs twice in development:** React StrictMode replayed setup. Make `onActivate` repeatable.
- **A long-running action cannot be cancelled by another action:** move that work into `fork` and retain its `ForkId`.
- **A state update overwrites newer input:** capture only effect inputs before waiting, then update current state with `modify_`.
- **Cleanup needs asynchronous work:** use an Aff finalizer in a component-owned fork; React cannot await an asynchronous deactivation callback.
- **An old event callback still fires:** Halo rejects its dispatch if the activation is stale, but the external source must still implement cleanup correctly.
