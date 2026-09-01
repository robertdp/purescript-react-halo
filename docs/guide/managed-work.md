# Managed work

Leave work in its action handler when no other action needs to identify it and rendering does not need a typed lifecycle. Add a task for renderable outcomes or a fork for an independent component process.

## Start with handler-owned work

A handler is already owned and cancelled by its React activation:

```purescript
Refresh -> do
  account <- lift Account.load
  modify_ _ { account = Just account }
```

No fork is needed merely because the application effect is asynchronous.

## Use a task for typed lifecycle state

A task field retains `Idle`, `Active`, a typed failure, or a typed result in component state:

```purescript
type State =
  { search :: Task.State SearchError Results
  , query :: String
  }

searchSlot :: Task.Slot "search" State SearchError Results
searchSlot = Task.slot (Proxy :: Proxy "search")

initialState =
  { search: Task.idle searchSlot
  , query: ""
  }
```

`Task.slot` uses its type-level label as both record field and identity. Use `slotAt` only for a nested or custom lawful focus:

```purescript
nestedSearchSlot =
  Task.slotAt (Proxy :: Proxy "nestedSearch")
    (tasksLens <<< searchLens)
```

One slot name must identify one focus for the runtime lifetime. Reusing a name for another lens—or giving one focus two names—is invalid and fails on first use before state mutation or cancellation:

```purescript
-- Invalid if `searchSlot` already names another focus.
duplicate = Task.slotAt (Proxy :: Proxy "search") otherLens
```

A task body is ordinary `HaloM` returning `Either error result`:

```purescript
Search query -> Task.supersede searchSlot do
  modify_ _ { query = query }
  lift (Search.run query)
```

A renderer receives `tasks :: Task.View State` beside component state:

```purescript
case Task.toStatus tasks searchSlot of
  Task.Idle -> renderPrompt
  Task.Active -> renderSpinner
  Task.Failed error -> renderError error
  Task.Succeeded results -> renderResults results
```

`Task.toMaybe tasks slot` returns only a successful result. `Task.isActive tasks slot` covers both a debounce timer and an executing body. The status prisms can inspect a projected status:

```purescript
result = preview Task._Succeeded (Task.toStatus tasks searchSlot)
```

State and task view come from one coherent render snapshot. `Task.State` is freely copyable, but a copied, stale, cross-slot, or cross-runtime active value has no matching authority and projects `Idle`; it cannot cross-cancel or commit over current work.

## Choose a task policy

Use `once` for initialization that remains terminal until reset:

```purescript
Task.once initializationSlot initialize
```

Use `startIfInactive` when another invocation should run after either success or typed failure, but not while work is active:

```purescript
Task.startIfInactive saveSlot save
```

Use `supersede` when every new invocation should become authoritative immediately:

```purescript
Task.supersede searchSlot (search query)
```

Use `debounce` for trailing-edge latest-wins interaction:

```purescript
Task.debounce searchSlot (Milliseconds 250.0) (search query)
```

Its private timer and body both render as `Active`. A new invocation cancels either phase.

Use `reset` to clear terminal state or cancel active work:

```purescript
Task.reset searchSlot
```

Reset publishes `Idle`, fences active work, and waits for its Aff finalizers. Supersession fences old work immediately but does not wait for its finalizers before starting the replacement.

Expected failures belong in `Either`. An unexpected exception returns the matching current task to `Idle` and follows Halo's `ForkError` reporting path. Cancellation is neither a typed failure nor an unexpected error.

Put retry policy in AppM and lift the retrying computation. Halo does not add retry, timestamps, or a generic scheduler to task state. Task state is component-owned result storage, not a global cache, and no computation or input is retained for reruns.

## Use a fork for an independent process

A fork has its own component-owned identity and may outlive the handler that starts it. Tasks and forks inherit the application interpreter captured by the handler that launches them:


```purescript
startSynchronization = do
  fiber <- Halo.fork synchronize
  modify_ _ { synchronization: Just fiber }
```

Retain its `ForkId` when another action must cancel it:

```purescript
cancelSynchronization = do
  current <- gets _.synchronization
  traverse_ Halo.kill current
  modify_ _ { synchronization = Nothing }
```

`kill` removes and fences the fork synchronously, then waits for Aff cancellation and finalizers. Killing an unknown or completed ID does nothing.

Use an Aff finalizer for asynchronous resource release inside owned work:

```purescript
connectionProcess = Halo.fork do
  liftAff $ Aff.finally useConnection closeConnection
```

React cannot await deactivation cleanup, but the ownership fence prevents a cancelled finalizer from committing Halo state or beginning another lifted application effect.

## Choose by observable behavior

| Requirement | Use |
|---|---|
| One action owns the result | handler |
| Rendering needs typed lifecycle state | task |
| A newer call replaces an older call | `supersede` or `debounce` |
| Work continues after its handler returns | fork |
| Cancellation must await finalizers | `reset` or `kill` |

Cancellation cannot retract an HTTP request, storage write, callback, or log that already happened. External writes still need appropriate idempotency or retry semantics.
