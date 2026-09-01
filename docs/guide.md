# Halo v4 guide

This guide explains Halo's action and first-class task model. Start with the [README quick start](../README.md); use the [API reference](reference.md) for exact signatures.

## Choose Halo for coordinated component workflows

Prefer ordinary React hooks when one asynchronous result follows render dependencies. Halo is useful when a component has an event protocol and several operations must share state and cancellation rules—for example, latest-request-wins search, duplicate-save prevention, ordered per-file uploads, or autosave that retains only the newest pending value.

Halo does not provide global state, server caching, a process/saga runtime, or server-side rendering machinery.

## Think in handlers, tasks, and structured children

### Handlers respond to events

Rendering code calls `dispatch :: action -> Effect Unit`. Halo immediately starts `handlers.onAction action` in the active component scope. Actions are not queued, deduplicated, or automatically treated as tasks.

```purescript
onAction = case _ of
  NameChanged name -> modify_ _ { name = name }
  SearchSubmitted query -> Halo.perform searchTask query
  SearchCleared -> Halo.cancel searchTask
```

Handlers are scope-owned and commit-fenced after deactivation, but their execution is not shown in `Activity`.

### Task definitions bind identity, strategy, and work

A task is a reusable value:

```purescript
searchTask = Halo.restartable SearchRequest \query -> do
  results <- liftAff $ fetchResults query
  modify_ _ { query = query, results = results }
```

Its type is `Task props state action key input`. The `key` identifies the scheduler slot; `input` is supplied separately on each `perform`. The smart constructor fixes the scheduling strategy so a call site cannot accidentally change concurrency behavior.

`perform task input` submits work and returns immediately. `perform_ task` is the `Unit`-input convenience form. Submitted work belongs to the active component scope, not to the handler or task that submitted it, so it can outlive successful caller completion. It may read props, update state, create structured children, or perform another task.

Define stable tasks near the workflow they implement and perform them from handlers. A function may return a task when the key itself is dynamic:

```purescript
uploadTask fileId = Halo.enqueue (Upload fileId) \chunk -> upload chunk

onAction (UploadChunk fileId chunk) =
  Halo.perform (uploadTask fileId) chunk
```

### Structured children stay with their parent

`fork child` starts concurrent work owned by the current handler or task. Unlike performed work, a forked child is cancelled when its parent finishes normally.

```purescript
refreshTask = Halo.restartable Refresh \_ -> do
  left <- Halo.fork loadLeftPane
  right <- Halo.fork loadRightPane
  waitUntilReady
  Halo.kill left
  Halo.kill right
```

Returning immediately after `fork` cancels the child. Use `perform` for component-scoped work that must survive the current handler; use `fork` for subwork whose lifetime must not exceed its parent.

## Configure handlers

```purescript
type Handlers props state action key =
  { onActivate :: HaloM props state action key Unit
  , onPropsChange :: props -> HaloM props state action key Unit
  , onAction :: action -> HaloM props state action key Unit
  }
```

Start with `defaultHandlers` and replace the fields you need:

```purescript
handlers = Halo.defaultHandlers
  { onActivate = initializeView
  , onPropsChange = \previous -> synchronize previous
  , onAction = handleAction
  }
```

### `onActivate`

Halo calls this for each React effect activation. Development StrictMode can run setup, cleanup, then setup again for one hook instance. Treat activation as repeatable. Activation work and tasks it performs are cancelled on deactivation.

### `onPropsChange previousProps`

Halo runs this when the props reference changes. The argument is the previous props; read current props with `Halo.getProps`. Halo always selects callbacks from the latest spec for new work.

### `onAction action`

Halo starts this when rendering code dispatches or a subscription emits an action. Dispatch while inactive is ignored. Long handler waits remain cancellable but are invisible to task activity; use a task when scheduling or renderable progress matters.

## Work with state and props

`HaloM props state action key` has `MonadState state`, `MonadEffect`, and `MonadAff` instances. Use ordinary `get`, `put`, and `modify_`. `Halo.getProps` reads the latest props.

When work becomes stale through replacement, cancellation, or deactivation, later Halo state operations cannot commit. Foreign effects that already occurred cannot be reversed. Capture render- or action-associated values as task input instead of relying on later props:

```purescript
submitTask = Halo.drop SubmitRequest submit

onAction Submit = do
  { form } <- Halo.getProps
  Halo.perform submitTask form
```

## Choose a scheduling strategy

Every task has a key, including concurrent tasks.

### `concurrent key implementation`

Every performance starts immediately, including multiple performances for the same key. Use this for independent bounded work such as metrics. A high-rate producer can create unbounded concurrency.

### `restartable key implementation`

A performance synchronously fences all running work for the key, discards its queue, requests cancellation, and starts the new input. Use it for search and navigation where the newest request wins.

### `drop key implementation`

A performance starts only when the key has no running or queued work. Otherwise its input is discarded and `perform` returns normally. Use it for optional duplicate submissions.

### `enqueue key implementation`

Every input is preserved FIFO and runs one at a time for the key. Different keys remain independent. The queue is unbounded, so bound the producer when input can exceed throughput.

### `keepLatest key implementation`

Current work may finish; only the newest queued input is retained. Intermediate queued inputs are discarded. Use it for autosave when in-flight writes should not be cancelled.

## Understand task identity and shared keys

The task value carries a key, but scheduling coordination is by key—not JavaScript object identity. Two definitions with the same key and strategy intentionally share one scheduler slot, cancellation boundary, and activity count. This supports separately named operations that must serialize together.

A key's first performed task establishes its strategy for the entire component runtime lifetime. Performing another definition with the same key and a different strategy is rejected: no work starts, and `onError` receives `TaskConfigurationError key` with an error naming both strategies. The association remains across StrictMode deactivate/reactivate cycles. This catches accidental key reuse while permitting deliberate same-strategy sharing.

Use distinct keys for independent work. Do not treat task input as identity: changing input creates another performance of the same task.

## Cancel a task

`cancel task` synchronously fences every running performance and discards every queued input for the task's key, requests fiber cancellation, publishes activity, and returns.

```purescript
onAction = case _ of
  SearchChanged query -> Halo.perform searchTask query
  SearchCleared -> do
    Halo.cancel searchTask
    modify_ _ { results = [] }
```

Definitions sharing a key share cancellation. If a task cancels its own key, it fences itself and all keyed siblings.

## Render task activity

`component` renderers and `useHalo` return `Activity key`:

```purescript
let
  searchCounts = Halo.activity searchTask halo.activity
  totalCounts = Halo.totalActivity halo.activity
```

Each count is `{ running, queued }`. Every task is keyed, so totals are the sum of all keyed slots. Definitions sharing a key report the same counts.

Activity includes only performed tasks. It excludes activation, prop-change, and action handlers, structured `fork` children, and subscription cleanup.

## Subscribe to custom emitters

Halo's emitter avoids a Halogen dependency:

```purescript
events = Halo.makeEmitter \emit -> do
  listener <- source.listen emit
  pure (source.remove listener)
```

`subscribe events` registers an action source in the current activation scope; `subscribeWithId (\id -> emitterFor id)` exposes the allocated ID during emitter setup; and `unsubscribe id` removes either form early. Manual unsubscription removes tracking before cleanup runs. Deactivation attempts every tracked cleanup even when one throws, then reports failures as `DeactivationError`.

Emitters broadcast without consuming-queue or backpressure semantics. Each emission dispatches an action; the handler may perform a task with the appropriate pressure strategy.

## Handle unexpected errors

Every spec supplies:

```purescript
onError :: ErrorContext props action key -> Error -> Effect Unit
```

Contexts are:

- `ActivationError` for `onActivate`;
- `PropsChangeError previousProps` for `onPropsChange`;
- `ActionError action` for `onAction`;
- `TaskError key` for performed task failure;
- `TaskConfigurationError key` for conflicting same-key strategies; and
- `DeactivationError` for throwing subscription cleanup.

Catch expected domain failures inside the task and put them in state or dispatch a domain action. Let unexpected failures reach `onError`. Halo-initiated cancellation is suppressed.

## Understand cancellation limits

Replacement, cancellation, and deactivation synchronously mark old owners stale, which blocks later Halo state commits and new Halo-owned capabilities, then request `Aff` cancellation. Cancellation is cooperative. It cannot retract an HTTP request, storage write, analytics event, or foreign callback already performed. Design external operations for retry, ordering, and idempotency where needed.

## Choose `component` or `useHalo`

Use `Halo.component` when Halo owns the whole component. Its renderer receives props, state, dispatch, and activity. `ComponentSpec.initialState` receives the initial props once per mount; synchronize later prop changes in `onPropsChange` rather than expecting state to be reinitialized.

Use `Halo.useHalo` when composing with other hooks:

```purescript
halo <- Halo.useHalo
  { props
  , initialState
  , handlers
  , onError
  }
```

Read `halo.state`, call `halo.dispatch`, and pass `halo.activity` to a task's `Halo.activity` helper.

## Common patterns

```purescript
searchTask = Halo.restartable SearchRequest search
saveTask = Halo.drop SaveRequest saveCurrentForm
uploadTask fileId = Halo.enqueue (Upload fileId) uploadChunk
autosaveTask = Halo.keepLatest AutosaveRequest saveDraft
metricTask = Halo.concurrent Metrics recordMetric

onAction = case _ of
  SearchChanged query -> Halo.perform searchTask query
  SaveClicked -> Halo.perform_ saveTask
  UploadChunk fileId chunk -> Halo.perform (uploadTask fileId) chunk
  DraftChanged draft -> Halo.perform autosaveTask draft
  MetricRecorded metric -> Halo.perform metricTask metric
```

## Troubleshooting and footguns

**My fork stops immediately.** Its parent returned. Perform a component-scoped task, or keep the parent alive while it owns the child.

**Activity is zero while work is running.** The work is probably in a handler or structured child. Only `perform`/`perform_` submissions count.

**A performed task was rejected with `TaskConfigurationError`.** Two definitions reuse a key with different strategies. Give independent work distinct keys or make deliberately shared definitions use one strategy.

**A dropped input did not run cleanup or report an error.** `drop` never starts the implementation when busy. Put only optional work behind it.

**My queue keeps growing.** `enqueue` is unbounded. Limit input, batch it, or use `keepLatest`/`drop`.

**A cancelled request still reached the server.** Halo fences component commits and requests cancellation; it cannot undo external effects.

**Initialization ran twice in development.** StrictMode replayed activation. Make `onActivate` replay-safe.

**An emitter overwhelms the component.** Reduce events at the source or perform a task with an appropriate pressure strategy.
