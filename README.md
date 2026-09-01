# React Halo

Halo gives a PureScript React component one typed action handler plus reusable, component-scoped tasks. It is for UI workflows where plain hooks become hard to coordinate: replace stale searches, prevent overlapping saves, preserve upload order, or retain only the newest pending refresh.

For one request derived directly from render dependencies, `React.Basic.Hooks.Aff.useAff` is usually simpler. Halo earns its place when actions, state transitions, cancellation, and task concurrency need one coherent owner.

## Mental model

Halo separates three kinds of work:

1. **Handlers** react to activation, prop changes, and dispatched actions. They start immediately, belong to the active React scope, and do not count as task activity.
2. **Tasks** are first-class definitions created with `concurrent`, `restartable`, `drop`, `enqueue`, or `keepLatest`. A definition binds its identity, scheduling strategy, and input-driven implementation. `perform` submits work that can outlive its caller and drives `Activity`.
3. **Structured children** are created with `fork`. A child belongs to its current handler or task and is cancelled when that parent finishes.

An action is an event, not an implicit task. The action handler decides whether to update state, perform or cancel a task, subscribe to events, or combine those operations.

## Try the unreleased v4

Halo v4 targets PureScript 0.15.16 and Spago 1.0.4. It is not published yet; the Registry still resolves `react-halo` to v3. Add a sibling checkout as a local package:

```yaml
package:
  dependencies:
    - react-basic-dom # only needed by this README's renderer
    - react-halo

workspace:
  extraPackages:
    react-halo:
      path: ../purescript-react-halo
```

After v4 is published:

```console
spago install react-halo react-basic-dom
```

Your application also needs the JavaScript packages required by `react-basic-hooks`, including React. Halo has no npm runtime entry point or npm runtime dependencies.

## Quick start: replace a stale request

Define the request once as a restartable task. Each click dispatches an action immediately; `perform_` then fences and cancels prior work for `GreetingRequest` before starting the new request.

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

data TaskKey = GreetingRequest

derive instance eqTaskKey :: Eq TaskKey
derive instance ordTaskKey :: Ord TaskKey

loadGreetingTask :: Halo.Task Props State Action TaskKey Unit
loadGreetingTask = Halo.restartable GreetingRequest \_ -> do
  modify_ _ { loading = true, result = Nothing }
  Props { loadGreeting } <- Halo.props
  outcome <- liftAff $ attempt loadGreeting
  modify_ _
    { loading = false
    , result = Just $ case outcome of
        Left error -> Left (message error)
        Right greeting -> Right greeting
    }

loadButton :: Component Props
loadButton = Halo.component "LoadButton"
  { initialState: \_ -> { loading: false, result: Nothing }
  , handlers: Halo.defaultHandlers
      { onAction = \Load -> Halo.perform_ loadGreetingTask }
  , onError: \_ error ->
      Console.error $ "Unexpected Halo failure: " <> message error
  , render: \{ state, dispatch, activity } ->
      let counts = Halo.activity loadGreetingTask activity
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

The task catches an expected domain failure and stores it in state. Unexpected failures that escape a handler or task go to `onError` with an `ErrorContext`.

## Learn and reference

- [Guide](docs/guide.md): handlers, task definitions, scheduling, cancellation, activity, subscriptions, activation, patterns, and troubleshooting.
- [API reference](docs/reference.md): public types and operations with exact semantics.
- [v3 to v4 migration](docs/migration-v4.md): breaking changes and a practical conversion sequence.

The important documentation examples compile in `test/Test/Halo/DocExamples.purs`.

## Development

```console
npm ci
npm run format:check
npm run build -- --strict --pedantic-packages
npm test
npx spago docs
```

The deterministic runtime tests model React's effect setup-cleanup-setup sequence directly. A DOM mounting test is intentionally omitted because this library's npm manifest contains only the pinned PureScript compiler and Spago; the hook uses the tested runtime boundary, and the component examples are compile-checked.
