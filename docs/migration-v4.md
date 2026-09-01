# Migrate from Halo v3 to v4

Halo v4 is an unreleased breaking redesign. It replaces the Free/FreeAp evaluator and implicit action effects with a direct scoped runtime, named handlers, and explicit tasks. There are no compatibility aliases in v4.

## Why the model changed

In v3, `eval` combined lifecycle events and actions, and action evaluation commonly became asynchronous work by convention. That made it difficult to tell whether an action was an event, a long-running task, or both. It also left concurrency policy and cancellation ownership implicit.

In v4:

- handlers respond to lifecycle and action events;
- `startTask` explicitly marks component-scoped asynchronous work;
- a `TaskPolicy` is chosen next to that work;
- `fork` is explicitly parent-scoped; and
- activity counts explicit tasks only.

## Migration sequence

### 1. Change `HaloM`

Replace:

```purescript
HaloM props state action m a
```

with:

```purescript
HaloM props state action key a
```

Choose an application task-key type with an `Ord` instance. Halo now runs directly on `Aff`; remove the custom base monad parameter, `hoist`, `HaloAp`, and Free/FreeAp-specific code. Use `liftAff` for asynchronous effects.

### 2. Replace `eval` with `handlers`

Replace lifecycle pattern matching:

```purescript
eval = case _ of
  Initialize -> initialize
  Update previous -> synchronize previous
  Action action -> handleAction action
  Finalize -> finalize
```

with:

```purescript
handlers = Halo.defaultHandlers
  { onActivate = initialize
  , onPropsChange = synchronize
  , onAction = handleAction
  }
```

There is no public `Lifecycle`, `EvalSpec`, `mkEval`, or `defaultEval` in v4.

`onActivate` is repeatable under React StrictMode. There is no asynchronous deactivation handler: React cleanup is synchronous, and pretending otherwise would give misleading completion guarantees. Use subscription cleanup, `Aff` finalizers, or an external resource owner.

### 3. Make tasks explicit

In v3, an action handler might perform a request directly:

```purescript
Action (SearchChanged query) -> do
  results <- liftAff $ search query
  modify_ _ { results = results }
```

In v4, submit work with its policy:

```purescript
onAction = case _ of
  SearchChanged query ->
    Halo.startTask (Halo.Restartable SearchRequest) do
      results <- liftAff $ search query
      modify_ _ { results = results }
```

Delete any top-level `schedule :: action -> TaskPolicy key`. An action is no longer implicitly a task. Some actions may only modify state; others may submit multiple tasks or cancel a keyed task.

### 4. Add explicit keyed cancellation where needed

Replace stored task fibers or cancellation actions with:

```purescript
Halo.cancelTask SearchRequest
```

This cancels running keyed tasks and discards their queue. It does not affect `Every` tasks.

### 5. Update the error handler

Change:

```purescript
onError :: ErrorContext props action -> Error -> Effect Unit
```

into:

```purescript
onError :: ErrorContext props action key -> Error -> Effect Unit
```

Handle the v4 contexts:

- `ActivationError`;
- `DeactivationError` for subscription cleanup;
- `PropsChangeError previousProps`;
- `ActionError action`; and
- `TaskError policy`.

Expected request failures still belong in domain state or actions.

### 6. Update hook and component specs

Remove `eval` and `schedule`; add `handlers`:

```purescript
halo <- Halo.useHalo
  { props
  , initialState
  , handlers
  , onError
  }
```

`useHalo` returns a record with `state`, `dispatch`, and `activity`.

`Halo.component` renderers receive `{ props, state, dispatch, activity }`. The old `send` field is now `dispatch`.

### 7. Revisit every `fork`

A v4 `fork` is a structured child. It is cancelled when its creating handler or task finishes. If the old code expected a fork to survive handler completion until component unmount, convert it to an explicit task:

```purescript
Halo.startTask (Halo.Restartable BackgroundSync) backgroundSync
```

Use `fork` only for concurrency owned by a parent that remains alive.

### 8. Replace Halogen emitters

Halo v4 has its own small emitter type:

```purescript
events = Halo.makeEmitter \emit -> do
  listener <- source.listen emit
  pure (source.remove listener)
```

`subscribe`, `subscribe'`, and `unsubscribe` remain. Manual cleanup is removed from tracking before it runs; scope cleanup failures are isolated and reported through `DeactivationError`.

## Behavior changes to verify

Before completing a migration, verify:

- `onActivate` is safe to replay;
- each long-running operation uses an intentional policy;
- `Drop` submissions are genuinely optional;
- `Enqueue` producers cannot grow an unbounded queue unexpectedly;
- `cancelTask` is used when UI state must clear keyed work without replacement;
- activity-dependent UI expects explicit tasks only;
- structured children do not need to outlive their parents;
- expected failures are modeled in state rather than logged as unexpected errors; and
- external writes remain correct even when local cancellation cannot undo them.
