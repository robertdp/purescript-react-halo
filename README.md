# React Halo

Halo gives a PureScript React component typed actions, local state, and a safe boundary for application effects. Application logic remains in its own monad; Halo owns the UI work started by a React component.

Use Halo when several interactions share state or asynchronous work must be cancelled with the component. For one request derived directly from render dependencies, `React.Basic.Hooks.Aff.useAff` is usually simpler.

## Choose a React entry point

Use `component` when Halo owns the complete component:

```purescript
profileComponent env =
  Halo.component "Profile" (runAppM env)
    { initialState
    , handlers
    , onError
    , render
    }
```

Use `useHalo` when the render function also uses other hooks:

```purescript
halo <- Halo.useHalo (runAppM env)
  { props
  , initialState
  , handlers
  , onError
  }

-- halo.state
-- halo.tasks
-- halo.dispatch
```

Both entry points receive an interpreter from the application's monad to `Aff`. They expose current component state, an immutable task view, and synchronous action dispatch. `component` also passes current props to its renderer.

## Understand the core model

Actions describe UI events, handlers perform component work, and rendering dispatches the next action:

```purescript
data Action
  = Rename String
  | LoadProfile

handlers = Halo.defaultHandlers
  { onAction = case _ of
      Rename name ->
        modify_ _ { name = name }

      LoadProfile -> do
        profile <- lift Profile.load
        modify_ _ { profile = Just profile }
  }
```

`HaloM props state action m` owns component state and props while preserving application capabilities in `m`:

```purescript
type UI a = Halo.HaloM Props State Action AppM a

newtype AppM a = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM program) = runReaderT program env
```

Standard transformer `lift` crosses that boundary. Each dispatched action starts an independent handler, so long-running handlers can overlap. React deactivation fences every handler before requesting cancellation.

Rendering reads state and dispatches actions synchronously:

```purescript
render { state, dispatch } =
  R.button
    { onClick: capture_ (dispatch LoadProfile)
    , children: [ R.text state.name ]
    }
```

## Choose the ownership mechanism

Start work directly in an action handler. Use a stronger mechanism only when the interaction needs it.

| Need | Use |
|---|---|
| Handle one UI event | action handler |
| Retain typed idle, active, failure, and success state | managed task |
| Let work outlive its launching handler | component-owned fork |
| Receive actions from an external event source | emitter subscription |
| Release another synchronous resource | registered cleanup |

A task stores a typed outcome in component state and renders through the matching task view:

```purescript
type State =
  { search :: Task.State SearchError Results
  }

searchSlot :: Task.Slot "search" State SearchError Results
searchSlot = Task.slot (Proxy :: Proxy "search")

Search query -> Task.supersede searchSlot do
  lift (Search.run query)

case Task.toStatus tasks searchSlot of
  Task.Idle -> renderPrompt
  Task.Active -> renderSpinner
  Task.Failed error -> renderError error
  Task.Succeeded results -> renderResults results
```

`Task.slot` uses one type-level label as both record field and identity. Use `Task.slotAt` only for a nested or custom lawful focus. Task bodies remain ordinary `HaloM` values returning `Either error result`.

A fork is an independently cancellable component process:

```purescript
fiber <- Halo.fork synchronize
Halo.kill fiber
```

Subscriptions turn external callbacks into actions:

```purescript
names = Halo.makeEmitter \emit -> source.listen emit
actions = NameChanged <$> names
void $ Halo.subscribe actions
```

Other synchronous resources can register cleanup directly:

```purescript
cleanupId <- Halo.registerCleanup removeListener
Halo.releaseCleanup cleanupId
```

React cleanup is synchronous. Put asynchronous release in an Aff finalizer owned by a handler, task, or fork rather than an `onDeactivate` callback.

## Install this unreleased version

The API on this branch is not published yet; the PureScript Registry currently resolves `react-halo` to v3. This branch uses the PureScript and Spago versions pinned in [`package.json`](package.json).

Add a checkout as a local Spago package and declare the dependencies used by your application:

```yaml
package:
  dependencies:
    - aff
    - console
    - effect
    - either
    - exceptions
    - prelude
    - react-basic-dom
    - react-basic-hooks
    - react-halo
    - transformers

workspace:
  extraPackages:
    react-halo:
      path: ../purescript-react-halo
```

After v4 is published, replace the local override with a registry installation:

```console
spago install aff console effect either exceptions prelude react-basic-dom react-basic-hooks react-halo transformers
```

`react-basic-dom` is used by the examples, not required by Halo itself. Applications also need the JavaScript packages required by `react-basic-hooks`, including React. Halo has no npm runtime entry point or npm runtime dependencies.

## Documentation

The [guide](docs/guide.md) covers complete usage and ownership choices. Generate exact API documentation from public source comments with `npx spago docs --offline`. Maintainers changing runtime ownership should also read the [architecture notes](docs/architecture.md) and [contributor guide](CONTRIBUTING.md).

Halo does not provide global state, server caching, backpressure queues, or a detached scheduler. The deterministic suite models React setup-cleanup-setup at the runtime boundary; the repository does not yet contain a real DOM/StrictMode mounting fixture.
