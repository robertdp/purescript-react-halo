# Halo v4 guide

This guide explains how to design a component around Halo's explicit action and task model. Start with the [README quick start](../README.md) if you have not built a Halo component yet. Exact signatures are in the [API reference](reference.md).

## Choose Halo for coordinated component workflows

React hooks remain the default. Prefer `useAff` when one asynchronous result follows render dependencies and latest-request cancellation is the only coordination you need.

Halo is useful when a component has a small event protocol and several operations must share state and cancellation rules. Typical examples include:

- search input where each request replaces the previous request;
- a save button that ignores duplicate clicks;
- per-file upload chunks that must preserve order;
- autosave where the current write may finish but only the newest pending value matters; and
- event sources that dispatch into the same state owner.

Halo does not provide global state, server caching, a process/saga runtime, or server-side rendering machinery.

## Think in actions, tasks, and structured children

### Actions describe events

Rendering code calls `dispatch :: action -> Effect Unit`. Halo starts `handlers.onAction action` in the active component scope. Every dispatched action gets a handler execution; actions are not deduplicated, queued, or assigned task policies.

Use an action handler for quick state transitions and decisions:

```purescript
onAction = case _ of
  NameChanged name -> modify_ _ { name = name }
  CancelSearch -> Halo.cancelTask SearchRequest
  SearchSubmitted query ->
    Halo.startTask (Halo.Restartable SearchRequest) (search query)
```

Handler execution is scope-owned and commit-fenced after deactivation, but it is not shown in `Activity`.

### Explicit tasks describe asynchronous work

`startTask policy computation` submits `computation` to the component task scheduler and returns immediately. The submitted task belongs to the active component scope, not to the handler that submitted it. It can therefore keep running after a successful action handler returns.

A task may read current props, update Halo state, create structured children, or submit another component-scoped task. Its state commits are fenced when it is replaced or cancelled.

Make task submission visible where the action is handled. Do not hide it behind a second action-to-policy table; the policy belongs next to the work whose concurrency it controls.

### Structured children describe parent-bound concurrency

`fork child` starts `child` concurrently under the current handler or task. Unlike an explicit task, a forked child is cancelled when its parent finishes normally. Use `fork` when the parent remains alive and owns concurrent subwork. Use `startTask` when work must outlive the action handler that launched it.

```purescript
Halo.startTask (Halo.Restartable Refresh) do
  left <- Halo.fork loadLeftPane
  right <- Halo.fork loadRightPane
  waitUntilReady
  Halo.kill left
  Halo.kill right
```

Returning immediately after `fork` cancels the child; it does not create a detached background process.

## Configure handlers

A spec has one cohesive `handlers` record:

```purescript
type Handlers props state action key =
  { onActivate :: HaloM props state action key Unit
  , onPropsChange :: props -> HaloM props state action key Unit
  , onAction :: action -> HaloM props state action key Unit
  }
```

Start with `defaultHandlers` and update only the fields you need:

```purescript
handlers = Halo.defaultHandlers
  { onActivate = initializeView
  , onPropsChange = \previous -> synchronize previous
  , onAction = handleAction
  }
```

### `onActivate`

Halo calls `onActivate` for each React effect activation. React development StrictMode can perform setup, cleanup, then setup again for the same hook instance. Treat activation as repeatable, not exactly once. Avoid irreversible “run once” effects unless the external owner supplies idempotency.

Activation execution is cancelled and commit-fenced on deactivation. If activation submits an explicit task, that task is also component-scoped and is cancelled on deactivation.

### `onPropsChange previousProps`

Halo runs this handler when the props reference changes. The argument is the previous props. Read current props inside Halo with:

```purescript
current <- Halo.props
```

The runtime keeps the latest handler record, error handler, and React update callbacks. A handler already running keeps the computation selected when it started.

### `onAction action`

Halo starts an action handler as soon as the action is dispatched into the active scope. A subscription emission also dispatches an action through this field. If the component is inactive, dispatch is ignored.

Long waits in an action handler remain cancellable and do not block other handlers, but they are invisible to `Activity`. Prefer an explicit task when running/queued state or a concurrency policy matters.

## Work with state and props

`HaloM props state action key` has `MonadState state`, `MonadEffect`, and `MonadAff` instances. Use normal state operations:

```purescript
current <- get
modify_ _ { status = Loading }
put next
```

Halo mirrors committed state into React. A replaced or deactivated owner can still finish foreign work, but later `get`/`put`/`modify_` operations cannot commit stale Halo state.

`Halo.props` reads the latest props rather than a render-time snapshot. Capture a value before starting a task when the task must use the value associated with the action:

```purescript
onAction Submit = do
  { form } <- Halo.props
  Halo.startTask (Halo.Drop SubmitRequest) (submit form)
```

## Select a task policy

### `Every`

Starts every submitted task immediately. Tasks run concurrently and appear only in total activity because `Every` has no key.

Use it for independent bounded work such as metrics. A high-rate producer can create unbounded concurrency.

### `Restartable key`

Synchronously fences all running tasks for `key`, discards queued tasks, requests cancellation, then starts the new task. Use it for latest-request-wins search and navigation.

Cancellation cannot undo an external effect that already happened. If server ordering matters, add idempotency or version checks at that boundary.

### `Drop key`

Starts the task only when `key` has no running or queued work. Otherwise the submission is discarded and `startTask` still returns normally. Use it to prevent duplicate form submissions.

### `Enqueue key`

Runs every task for `key` first-in, first-out, one at a time. Different keys remain independent. Use it for ordered writes or per-resource uploads.

The queue is unbounded. Bound the producer or choose another policy when sustained input can exceed throughput.

### `KeepLatest key`

Lets the current task finish, keeps only the newest queued task, and discards intermediate queued submissions. Use it for autosave when cancelling an in-flight write is undesirable but stale pending writes have no value.

Use one stable policy for a given key. Mixing policies is processed according to each arriving submission, but it makes the workflow harder to reason about.

## Cancel keyed tasks explicitly

`cancelTask key` immediately fences running tasks for the key, discards its queue, requests fiber cancellation, updates activity, and returns. It does not affect unkeyed `Every` tasks.

```purescript
onAction = case _ of
  SearchChanged query ->
    Halo.startTask (Halo.Restartable SearchRequest) (search query)
  SearchCleared -> do
    Halo.cancelTask SearchRequest
    modify_ _ { results = [] }
```

If a keyed task cancels its own key, it fences and requests cancellation of itself as well as its keyed siblings.

## Render task activity

`component` renderers and `useHalo` return `Activity key`. Activity updates cause React renders.

```purescript
let
  search = Halo.activityFor SearchRequest halo.activity
  total = Halo.activityTotals halo.activity
```

Each count has `{ running, queued }`. Totals include keyed tasks and unkeyed `Every` tasks. Per-key counts include keyed tasks only.

Activity deliberately excludes:

- activation, prop-change, and action handlers;
- structured `fork` children; and
- emitter subscription cleanup.

This keeps the value precise: it represents only work submitted through `startTask`.

## Subscribe to custom emitters

Halo's small emitter type avoids a Halogen dependency:

```purescript
events :: Halo.Emitter Action
events = Halo.makeEmitter \emit -> do
  listener <- source.listen emit
  pure (source.remove listener)
```

Registration receives an action callback and returns an `Effect Unit` cleanup. Subscribe inside Halo:

```purescript
subscriptionId <- Halo.subscribe events
Halo.unsubscribe subscriptionId
```

A subscription remains component-scoped after the creating handler finishes. Manual unsubscription removes cleanup from tracking before running it. Deactivation attempts every tracked cleanup, even when one throws, and reports each thrown cleanup as `DeactivationError` after cancellation requests have been issued.

`Emitter` is broadcast-style. It is not a consuming queue and does not provide backpressure. Each emission dispatches one action to each registered Halo receiver. Choose task policies inside `onAction` when emitted actions start asynchronous work.

## Handle unexpected errors

Every spec supplies:

```purescript
onError :: ErrorContext props action key -> Error -> Effect Unit
```

Contexts are:

- `ActivationError` for `onActivate`;
- `PropsChangeError previousProps` for `onPropsChange`;
- `ActionError action` for `onAction`;
- `TaskError policy` for an explicit task; and
- `DeactivationError` for a throwing subscription cleanup.

Expected failures belong in domain state or actions. Catch them inside the task with `attempt`, `try`, or a domain-specific error type. Let genuinely unexpected failures reach `onError` for logging or reporting. Cancellation requested by Halo is suppressed rather than reported as an application error.

## Understand cancellation limits

Replacement and deactivation do two things:

1. mark the old owner inactive synchronously, blocking later Halo state commits and new Halo-owned capabilities; and
2. request cancellation of its `Aff` fibers.

Cancellation is cooperative. It cannot retract an HTTP request, storage write, analytics event, or foreign callback already performed. Commit fencing protects Halo state, not external systems. Design external operations for retry, ordering, and idempotency when those properties matter.

## Choose `component` or `useHalo`

Use `Halo.component` when Halo owns the full component. The renderer receives props, state, dispatch, and activity.

Use `Halo.useHalo` when composing with other hooks:

```purescript
halo <- Halo.useHalo
  { props
  , initialState
  , handlers
  , onError
  }
```

Read `halo.state`, call `halo.dispatch`, and render `halo.activity`.

## Common patterns

### Search with latest-request-wins

Capture the query from the action and use `Restartable`:

```purescript
SearchChanged query ->
  Halo.startTask (Halo.Restartable SearchRequest) do
    results <- liftAff $ fetchResults query
    modify_ _ { query = query, results = results }
```

### Ignore duplicate saves

```purescript
SaveClicked ->
  Halo.startTask (Halo.Drop SaveRequest) saveCurrentForm
```

### Ordered work per resource

```purescript
UploadChunk fileId chunk ->
  Halo.startTask (Halo.Enqueue (Upload fileId)) (upload chunk)
```

### Cancel when input becomes empty

```purescript
QueryChanged "" -> Halo.cancelTask SearchRequest
QueryChanged query ->
  Halo.startTask (Halo.Restartable SearchRequest) (search query)
```

## Troubleshooting and footguns

**My fork stops immediately.** Its parent returned. Use `startTask` for component-scoped work, or keep the parent alive while it owns the child.

**Activity is zero while work is running.** The work is probably in a handler or structured child. Submit it with `startTask` if it is task activity.

**A dropped task did not run an error or completion action.** `Drop` intentionally discards the computation when its key is busy. Put only optional work behind it, or reflect acceptance separately in state.

**My queue keeps growing.** `Enqueue` has no built-in bound. Limit input, batch it, or use `KeepLatest`/`Drop`.

**A cancelled request still reached the server.** Halo can fence component commits and request `Aff` cancellation; it cannot undo an external side effect.

**Initialization ran twice in development.** React StrictMode replayed effect activation. Make `onActivate` replay-safe.

**An emitter overwhelms the component.** Emitters broadcast without backpressure. Reduce events at the source or let actions submit tasks with a pressure-appropriate policy.
