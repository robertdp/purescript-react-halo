# Migrate from Halo v3 to v4

Halo v4 is an unreleased breaking redesign. It replaces the Free/FreeAp evaluator and implicit action effects with a direct scoped runtime, named handlers, and first-class tasks. There are no compatibility aliases.

## Why the model changed

In v3, `eval` combined lifecycle events and actions, and asynchronous action work was conventional rather than explicit. It was difficult to tell whether an action was an event, a long-running task, or both, and concurrency ownership was easy to obscure.

In v4:

- handlers respond to activation, prop changes, and actions;
- reusable task definitions bind identity, scheduling strategy, and implementation;
- `perform` explicitly starts component-scoped work;
- `fork` is explicitly parent-scoped; and
- activity counts performed tasks only.

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

Task input does not become another `HaloM` parameter. It is generic on each `Task` value.

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

`onActivate` is repeatable under React StrictMode. There is no asynchronous deactivation handler: React cleanup is synchronous. Use subscription cleanup, `Aff` finalizers, or an external resource owner.

### 3. Define long-running operations as tasks

Create a key type, then define each task once:

```purescript
data TaskKey = SearchRequest | SaveRequest

derive instance eqTaskKey :: Eq TaskKey
derive instance ordTaskKey :: Ord TaskKey

searchTask :: Halo.Task Props State Action TaskKey String
searchTask = Halo.restartable SearchRequest \query -> do
  results <- liftAff $ search query
  modify_ _ { results = results }

saveTask :: Halo.Task Props State Action TaskKey Unit
saveTask = Halo.drop SaveRequest \_ -> saveCurrentForm
```

The available constructors are `concurrent`, `restartable`, `drop`, `enqueue`, and `keepLatest`. Each takes a key and an input-driven implementation. Strategy is part of the definition and cannot vary at performance sites.

A key's first performance establishes its strategy for the component runtime lifetime. Deliberate same-key, same-strategy definitions share a slot. Conflicting same-key strategies are rejected through `TaskConfigurationError key`.

### 4. Perform tasks from actions

Replace direct long-running action work or any action-to-policy table with:

```purescript
onAction = case _ of
  SearchChanged query -> Halo.perform searchTask query
  SaveClicked -> Halo.perform_ saveTask
```

An action is no longer implicitly a task. Some actions only update state; others may perform multiple tasks.

### 5. Replace keyed cancellation and activity lookup

Cancellation now takes the task definition:

```purescript
Halo.cancel searchTask
```

This fences running work and discards queued work for the task's key. Same-key definitions share cancellation.

Activity lookup also takes the task:

```purescript
searchCounts = Halo.activity searchTask halo.activity
totalCounts = Halo.totalActivity halo.activity
```

Every task, including `concurrent`, is keyed. Replace direct key lookup with the task-based helper.

### 6. Update the error handler

Change:

```purescript
onError :: ErrorContext props action -> Error -> Effect Unit
```

into:

```purescript
onError :: ErrorContext props action key -> Error -> Effect Unit
```

Handle:

- `ActivationError`;
- `DeactivationError` for subscription cleanup;
- `PropsChangeError previousProps`;
- `ActionError action`;
- `TaskError key`; and
- `TaskConfigurationError key`.

Expected request failures still belong in domain state or actions.

### 7. Update hook and component specs

Remove `eval` and `schedule`; add `handlers`:

```purescript
halo <- Halo.useHalo
  { props
  , initialState
  , handlers
  , onError
  }
```

`useHalo` returns `state`, `dispatch`, and `activity`. `Halo.component` renderers receive `{ props, state, dispatch, activity }`; the old `send` field is now `dispatch`.

### 8. Revisit every `fork`

A v4 `fork` is a structured child. It is cancelled when its creating handler or task finishes. If old code expected a fork to survive handler completion, make it a task and call `perform`:

```purescript
backgroundSync = Halo.restartable BackgroundSync \_ -> synchronize

onAction StartSync = Halo.perform_ backgroundSync
```

Use `fork` only for concurrency owned by a parent that remains alive.

### 9. Replace Halogen emitters

Halo v4 has its own emitter type:

```purescript
events = Halo.makeEmitter \emit -> do
  listener <- source.listen emit
  pure (source.remove listener)
```

`subscribe` and `unsubscribe` remain; use the semantic name `subscribeWithId` when emitter setup needs the allocated ID. Manual cleanup is removed from tracking before it runs; scope cleanup failures are isolated and reported through `DeactivationError`.

## Behavior changes to verify

Before completing a migration, verify:

- `onActivate` is safe to replay;
- task keys are stable and distinct where work is independent;
- definitions sharing a key use one intentional strategy;
- `drop` inputs are genuinely optional;
- `enqueue` producers cannot grow an unbounded queue unexpectedly;
- `cancel task` is used when UI state must clear work without replacement;
- activity-dependent UI expects performed tasks only;
- structured children do not need to outlive parents;
- expected failures are modeled in state rather than logged as unexpected errors; and
- external writes remain correct even when local cancellation cannot undo them.
