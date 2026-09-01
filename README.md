# React Halo

Halo gives PureScript React components one typed action loop for state and asynchronous effects, with component-scoped cancellation and explicit concurrency policies.

Use Halo when a component has event-driven workflows that are awkward to express as independent hooks: rapid searches that must replace stale requests, saves that must not overlap, ordered uploads, or bursts where only the newest pending action matters. For a single request derived directly from render dependencies, `React.Basic.Hooks.Aff.useAff` is usually simpler.

## Install

Halo v4 targets PureScript 0.15.16 and Spago 1.0.4. It is not published yet: the Registry still resolves `react-halo` to v3. To try v4 from a sibling checkout, add Halo as a local package and include `react-basic-dom` for the quick-start renderer:

```yaml
package:
  dependencies:
    - react-basic-dom
    - react-halo

workspace:
  extraPackages:
    react-halo:
      path: ../purescript-react-halo
```

After v4 is published, install both packages with:

```console
spago install react-halo react-basic-dom
```

Halo itself does not require `react-basic-dom`; only the example renderer does. Your application also needs the JavaScript packages required by `react-basic-hooks`, including React. Halo does not publish an npm runtime entry point.

## Quick start: a restartable request

This complete component starts `loadGreeting` when the button is clicked. Clicking again while the request is running cancels the previous Halo task. Even if underlying work cannot be interrupted, the replaced task cannot commit Halo state.

```purescript
module Example.LoadButton where

import Prelude

import Control.Monad.State (modify_)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, attempt)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console as Console
import Effect.Exception (message)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (Component)
import React.Halo as Halo

newtype Props = Props { loadGreeting :: Aff String }

type State =
  { loading :: Boolean
  , result :: Maybe (Either String String)
  }

data Action = Load

data Task = GreetingRequest

derive instance eqTask :: Eq Task
derive instance ordTask :: Ord Task

loadButton :: Component Props
loadButton = Halo.component "LoadButton"
  { initialState: \_ -> { loading: false, result: Nothing }
  , schedule: \Load -> Halo.Restartable GreetingRequest
  , eval: case _ of
      Halo.Action Load -> do
        modify_ _ { loading = true, result = Nothing }
        Props { loadGreeting } <- Halo.props
        outcome <- liftAff $ attempt loadGreeting
        modify_ _
          { loading = false
          , result = Just $ case outcome of
              Left error -> Left (message error)
              Right greeting -> Right greeting
          }
      _ -> pure unit
  , onError: \_ error ->
      Console.error $ "Unexpected Halo failure: " <> message error
  , render: \{ state, dispatch, activity } ->
      let counts = Halo.activityFor GreetingRequest activity
      in R.div_
        [ R.button
            { onClick: capture_ (dispatch Load)
            , children:
                [ R.text if counts.running > 0 then "Restart load" else "Load" ]
            }
        , R.text $ case state.result of
            Nothing -> if state.loading then "Loading…" else "Not loaded"
            Just (Left error) -> error
            Just (Right greeting) -> greeting
        ]
  }
```

The example catches an expected request failure and stores it in domain state. `onError` is for unexpected failures that escape `eval`.

## Schedule actions by intent

The `schedule` function assigns each dispatched action a policy. Keys are an application-defined type with an `Ord` instance; actions with the same key coordinate with one another.

| Policy | Behavior |
| --- | --- |
| `Every` | Start every action immediately and run them concurrently. It has no key. |
| `Restartable key` | Fence and cancel all running work for `key`, discard its queue, and start the new action. |
| `Drop key` | Ignore the new action while work for `key` is running or queued. |
| `Enqueue key` | Run every action for `key` in first-in, first-out order, one at a time. |
| `KeepLatest key` | Let the running action finish, retain only the newest pending action, and discard intermediate pending actions. |

A realistic scheduler remains a small pattern match:

```purescript
data Action
  = SearchChanged String
  | SaveClicked
  | Autosave String
  | UploadChunk Int Int
  | RecordMetric String

data Task
  = SearchRequest
  | SaveRequest
  | AutosaveRequest
  | Upload Int

derive instance eqTask :: Eq Task
derive instance ordTask :: Ord Task

schedule :: Action -> Halo.TaskPolicy Task
schedule = case _ of
  SearchChanged _ -> Halo.Restartable SearchRequest
  SaveClicked -> Halo.Drop SaveRequest
  Autosave _ -> Halo.KeepLatest AutosaveRequest
  UploadChunk fileId _ -> Halo.Enqueue (Upload fileId)
  RecordMetric _ -> Halo.Every
```

Use one stable policy for a given key. Mixing policies on one key is defined by each arriving action, but is harder to reason about.

`Every` can create unbounded concurrent work, and `Enqueue` can create an unbounded queue if producers are faster than consumers. Use `Drop` or `KeepLatest`, or bound input at its source, when load can spike.

### Render activity

`useHalo` and `component` return an `Activity key` snapshot. Activity changes trigger a React render.

```purescript
let
  search = Halo.activityFor SearchRequest activity
  total = Halo.activityTotals activity

in R.text $
  show search.running <> " search running, " <>
  show total.queued <> " total queued"
```

`activityFor` reports `{ running, queued }` for one keyed task. `activityTotals` includes all keyed work and unkeyed `Every` work. Lifecycle evaluations and structured child fibers are not included.

## Lifecycle and cancellation

The evaluator receives:

```purescript
data Lifecycle props action
  = Activate
  | Update props -- previous props
  | Action action
```

`Activate` does **not** mean “exactly once.” React may run an effect setup, cleanup, and setup again for the same hook instance in development StrictMode. Halo treats each setup as a fresh active scope. Deactivation cancels that scope's action evaluations, queued work, lifecycle evaluations, structured children, and subscriptions; a later activation is usable again.

`Update previousProps` runs when the props reference changes. Read current props with `Halo.props`. Halo keeps the latest evaluator, scheduler, error handler, and React update callbacks rather than permanently capturing the initial hook spec.

There is no `Finalize` evaluator in v4. React cleanup is synchronous, so asynchronous finalizers would have misleading guarantees. Put external resources behind `subscribe` cleanup, an `Aff` bracket/finalizer, or another resource owner with explicit semantics.

The task policy applies to dispatched actions, including actions emitted by subscriptions. `Activate` and `Update` evaluations are scope-owned but do not pass through `schedule`. If initialization should use a task policy, dispatch an ordinary action from the application boundary rather than hiding long-running work in lifecycle logic.

### What cancellation guarantees

Halo performs two operations on replacement or deactivation:

1. It marks the old owner inactive immediately, blocking later Halo state commits and capability acquisition.
2. It requests cancellation of the owned `Aff` fibers.

Cancellation cannot undo an HTTP request already sent, a log already written, or any other external effect already performed. Some foreign async APIs also cannot be interrupted. Model idempotency and server-side concurrency where correctness requires them; Halo's commit fence only protects the component's Halo state from stale work.

`fork` creates a structured child of the current evaluation. The child is cancelled when its parent finishes, is replaced, or is deactivated. Use it only for concurrency within that evaluation, and use `kill` for earlier cancellation. Returning from the parent is not a way to create a detached component process.

## Subscriptions

Halo has a small emitter type rather than depending on Halogen. Registration receives an action callback and must return that receiver's cleanup effect:

```purescript
eventEmitter = Halo.makeEmitter \emit -> do
  listener <- source.listen emit
  pure (source.remove listener)

Halo.Action StartListening -> do
  subscriptionId <- Halo.subscribe eventEmitter
  modify_ _ { subscriptionId = Just subscriptionId }

Halo.Action StopListening -> do
  { subscriptionId } <- get
  traverse_ Halo.unsubscribe subscriptionId
  modify_ _ { subscriptionId = Nothing }
```

Manual `unsubscribe` removes the cleanup from Halo's tracking before running it. Any cleanup still tracked at deactivation is run automatically. One cleanup failure is reported through `onError` only after Halo has attempted every subscription cleanup and requested cancellation of all other scope-owned work. New subscriptions from stale or inactive evaluations are rejected, and callbacks retained by a misbehaving source remain bound to their original scope rather than targeting a later reactivation.

An `Emitter` is broadcast-style: every subscriber receives every emitted value. It is not a consuming work queue and provides no backpressure. Each event delivered to Halo is dispatched once and then follows its action policy. Halo v4 intentionally does not expose a coroutine, process, or saga API; task scheduling is the focused concurrency boundary.

## Error handling

Every spec must provide:

```purescript
onError :: Halo.ErrorContext props action -> Error -> Effect Unit
```

The context is `ActivationError`, `DeactivationError`, `UpdateError previousProps`, or `ActionError action`. `DeactivationError` reports a subscription cleanup that threw; Halo continues cleaning the rest of the scope before calling `onError`. Expected domain failures belong in the action/state model, usually by catching `Aff` errors inside `eval`. Unexpected uncaught errors go to `onError`. Cancellation caused by replacement or deactivation is suppressed rather than reported as an application failure.

## Component helper or hook

Use `Halo.component` when Halo owns the whole component. Its renderer receives:

```purescript
{ props :: props
, state :: state
, dispatch :: action -> Effect Unit
, activity :: Halo.Activity key
}
```

Use `Halo.useHalo` when composing Halo with other React hooks:

```purescript
halo <- Halo.useHalo
  { props
  , initialState
  , eval
  , schedule
  , onError
  }

-- halo.state
-- halo.dispatch
-- halo.activity
```

`HaloM props state action key` has `MonadState state`, `MonadEffect`, and `MonadAff` instances. Use normal `get`, `put`, and `modify_`; use `liftAff` for asynchronous work. `Halo.props` reads the latest props.

`mkEval` remains available for simple lifecycle-to-action routing:

```purescript
eval = Halo.mkEval $ Halo.defaultEval
  { initialize = Just InitializeData
  , handleAction = handleAction
  }
```

The `update` field can map previous props to an optional action. These lifecycle-routed actions execute inside the lifecycle evaluation; they are not independently scheduled.

## Migrating from v3

Version 4 intentionally breaks the evaluator API to make cancellation and ownership reliable.

- Change `HaloM props state action m` to `HaloM props state action key`. Halo now runs directly on `Aff`; remove `hoist`, `HaloAp`, and the custom base monad parameter.
- Add an application task-key type with `Eq` and `Ord`, then add `schedule :: action -> TaskPolicy key`.
- Add `onError :: ErrorContext props action -> Error -> Effect Unit`, including the new `DeactivationError` context for subscription cleanup failures.
- Replace `Initialize` with `Activate`. `Activate` is repeatable.
- Remove `Finalize` handlers. Use scoped cancellation, subscriptions, and `Aff` finalizers instead.
- Keep `Update previousProps`, and read current props with `Halo.props`.
- Replace the `useHalo` tuple with the record fields `state`, `dispatch`, and `activity`.
- In `component` renderers, rename `send` to `dispatch` and accept `activity` when needed.
- Revisit `fork`: v4 children are structured under the evaluation that created them, not detached until component unmount.
- Replace `Halogen.Subscription.Emitter` values with `Halo.makeEmitter`; the registration function has the same callback-and-cleanup shape but no Halogen dependency.
- Remove assumptions that action effects run without coordination. Choose `Every` explicitly for v3-like concurrent dispatch.

## Development

Install the pinned tools and run the checks:

```console
npm ci
npm run format:check
npm run build -- --strict
npm test
```

The runtime tests model React's effect setup-cleanup-setup sequence directly and use deterministic `AVar` gates for scheduling and cancellation. A DOM mounting test is intentionally omitted because this package's npm manifest contains only the PureScript compiler and Spago; the repeatable lifecycle contract is tested at the runtime boundary used by the hook.

Module documentation is generated by PureScript and can be published to [Pursuit](https://pursuit.purescript.org/packages/purescript-react-halo) with a release.
