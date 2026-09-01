# Halo v4 guide

Halo combines a typed UI action handler with component state and a runtime boundary for your application monad. This guide starts from the [README quick start](../README.md); use the [API reference](reference.md) for exact signatures.

## Keep application logic in `AppM`

Most applications already have a monad that carries services or configuration over `Aff`:

```purescript
newtype AppM a = AppM (ReaderT AppEnv Aff a)

runAppM :: AppEnv -> AppM ~> Aff
runAppM env (AppM program) = runReaderT program env
```

Halo restores that monad as the fourth `HaloM` parameter:

```purescript
HaloM props state action AppM result
```

Use the standard transformer operation to run application logic:

```purescript
import Control.Monad.Trans.Class (lift)

loadAccount :: UI Unit
loadAccount = do
  account <- lift Account.load
  modify_ _ { account = Just account }
```

The interpreter is explicit at the React boundary:

```purescript
Halo.component "Account" (runAppM env) spec
Halo.useHalo (runAppM env) hookSpec
```

A handler or fork keeps the interpreter with which it started. If a later render supplies another interpreter, only new roots use it. Do not implement an interpreter by detaching work with `launchAff_`: Halo can only own and cancel the `Aff` returned by the interpreter.

## Handle an action ADT

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

Actions are concurrent by default, matching the event-driven model: one long-running action does not block later dispatches. Work is still cancelled when the React scope deactivates. Use a component-owned fork when another action needs an ID with which to cancel a process.

## Work with state and props

`HaloM` has `MonadState state`. Use normal `get`, `put`, `gets`, `modify`, and `modify_` operations.

`Halo.getProps` reads the latest props. `onPropsChange` receives the previous props, so both sides of a synchronization are available:

```purescript
onPropsChange = \previous -> do
  current <- Halo.getProps
  synchronize previous current
```

State commits are fenced. After a handler or fork is killed, or after its activation deactivates, later Halo state operations can still compute their return value but cannot commit a new state or call React's state setter.

Capture values associated with an action before starting work when they must not change during that work. `getProps` intentionally reads current props rather than a render snapshot.

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

`Halo.kill id` removes the fork from component tracking, fences its state and capabilities synchronously, requests Aff cancellation, and waits for cancellation and Aff finalizers before returning. Killing an unknown or completed ID does nothing.

Deactivation cannot wait asynchronously because React cleanup is synchronous. It fences the whole activation first, attempts every subscription cleanup, and requests cancellation of all remaining handlers and forks. Aff finalizers continue in their cancellation fibers, but they cannot commit Halo state.

Cancellation is cooperative. It cannot retract an HTTP request, storage write, callback, or log that already happened. Design external writes for retry and idempotency where needed.

## Run independent work in parallel

`HaloM` has a direct `Parallel` instance with abstract counterpart `HaloAp`. Branches share the same root, scope, and interpreter snapshot:

```purescript
loadDashboard = do
  Tuple profile feed <- sequential ado
    profile <- parallel (lift Profile.load)
    feed <- parallel (lift Feed.load)
    in Tuple profile feed

  modify_ _ { profile = profile, feed = feed }
```

Prefer parallel application reads followed by one Halo state commit. Concurrent Halo state writes have nondeterministic ordering; a later commit can overwrite an earlier one.

Parallel work is lexical: the combined computation waits for its branches. Use `fork` only when work must continue independently of the launching handler or needs explicit cancellation by ID.

## Configure lifecycle handlers

```purescript
type Handlers props state action m =
  { onActivate :: HaloM props state action m Unit
  , onPropsChange :: props -> HaloM props state action m Unit
  , onAction :: action -> HaloM props state action m Unit
  }
```

Start with `defaultHandlers` and replace only what the component needs.

### `onActivate`

Halo calls this for every React effect activation. Development StrictMode can run setup, cleanup, then setup again for one hook instance. Treat activation as repeatable. Work from a prior activation is fenced and cancelled before a new activation becomes current.

### `onPropsChange previousProps`

Halo starts this when the props reference changes. Read current props with `getProps`. New prop-change roots use the latest handlers and interpreter supplied by the hook.

### `onAction action`

Halo starts one root for every action dispatched while active, including actions emitted by subscriptions. Dispatch while inactive is ignored.

There is no asynchronous deactivation callback. Use subscriptions, Aff finalizers, or an external resource owner with explicit cleanup semantics.

## Subscribe to custom emitters

Halo's emitter avoids a Halogen dependency:

```purescript
events = Halo.makeEmitter \emit -> do
  listener <- source.listen emit
  pure (source.remove listener)
```

`subscribe events` registers an action source in the current activation. `subscribeWithId (\id -> emitterFor id)` exposes the allocated `SubscriptionId` during registration. `unsubscribe id` removes tracking before running cleanup.

Deactivation attempts every tracked cleanup even when one throws. Cleanup failures are reported as `DeactivationError` only after Halo has requested cleanup for the rest of the scope. A callback retained by a faulty source remains bound to its original activation and cannot dispatch into a later StrictMode reactivation.

Emitters broadcast actions. They do not provide backpressure, consuming queues, or scheduling policies.

## Handle unexpected errors

Every spec supplies:

```purescript
onError :: ErrorContext props action -> Error -> Effect Unit
```

Contexts are:

- `ActivationError` for `onActivate`;
- `PropsChangeError previousProps` for `onPropsChange`;
- `ActionError action` for `onAction`;
- `ForkError id` for a component-owned fork; and
- `DeactivationError` for throwing subscription cleanup.

Halo selects the latest `onError` callback when an unexpected failure is reported. Expected domain failures belong in application values or Halo state. Cancellation initiated by Halo is suppressed because the root has already been fenced.

## Choose `component` or `useHalo`

Use `Halo.component` when Halo owns the whole component. The renderer receives `{ props, state, dispatch }`. `initialState` receives initial props once per mount; synchronize later changes in `onPropsChange`.

Use `Halo.useHalo` when other React hooks share the render function. It accepts the same interpreter and returns `{ state, dispatch }`.
