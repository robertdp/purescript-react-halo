# React Halo

Halo gives a PureScript React component one typed action handler, component state, and a safe boundary for application effects. Define UI interactions with an action ADT, lift your application monad into `HaloM`, and supply an interpreter from that monad to `Aff` when the component or hook is created.

Each active React effect owns its handlers, component forks, and subscriptions. Deactivation cancels that work, and work that has been killed or deactivated cannot commit Halo state.

For one request derived directly from render dependencies, `React.Basic.Hooks.Aff.useAff` is usually simpler. Halo is useful when actions, shared state transitions, application logic, and cancellable component processes need one coherent owner.

## Mental model

1. **Actions** are values in your UI action ADT. Rendering code calls `dispatch :: action -> Effect Unit`; Halo starts the action handler in the active component scope.
2. **Application effects** remain in your application monad, commonly `ReaderT AppEnv Aff`. Use the standard transformer `lift` inside `HaloM`. The interpreter supplied to `component` or `useHalo` runs those effects in Halo-owned `Aff` fibers.
3. **Forks** are component-owned processes. A fork may outlive the handler that started it, can be killed by its `ForkId`, and is cancelled when the React scope deactivates.

Halo does not provide global state, server caching, or a separate process runtime.

## Try the unreleased v4

Halo v4 targets PureScript 0.15.16 and Spago 1.0.4. It is not published yet; the Registry still resolves `react-halo` to v3. Add a sibling checkout as a local package:

```yaml
package:
  dependencies:
    - aff
    - console
    - effect
    - either
    - exceptions
    - foldable-traversable
    - maybe
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

After v4 is published:

```console
spago install aff console effect either exceptions foldable-traversable maybe prelude react-basic-dom react-basic-hooks react-halo transformers
```

This list is complete for the quick-start shape under Spago's pedantic dependency check; an existing React application will already declare several packages. `react-basic-dom` is required by the renderer, not by Halo. Your application also needs the JavaScript packages required by `react-basic-hooks`, including React. Halo has no npm runtime entry point or npm runtime dependencies.

## Quick start

Define the application monad and its runtime interpreter:

```purescript
newtype AppM a = AppM (ReaderT Env Aff a)

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM program) = runReaderT program env
```

Use an action ADT for interactions and keep cancellation identity in component state:

```purescript
type State =
  { fiber :: Maybe Halo.ForkId
  , loading :: Boolean
  , result :: Maybe String
  }

data Action = Load | Cancel

type UI a = Halo.HaloM Props State Action AppM a

handlers :: Halo.Handlers Props State Action AppM
handlers = Halo.defaultHandlers
  { onAction = case _ of
      Load -> do
        previous <- gets _.fiber
        traverse_ Halo.kill previous
        fiber <- Halo.fork do
          modify_ _ { loading = true, result = Nothing }
          result <- lift loadGreeting
          modify_ _ { loading = false, result = Just result }
        modify_ _ { fiber = Just fiber }

      Cancel -> do
        previous <- gets _.fiber
        traverse_ Halo.kill previous
        modify_ _ { fiber = Nothing, loading = false }
  }
```

`lift` is `Control.Monad.Trans.Class.lift`. The `AppM` value runs through the interpreter captured when that handler or fork started. A new render may supply a new interpreter for later roots without changing one already running.

Create the component at the application boundary:

```purescript
loadButton :: Env -> Component Props
loadButton env = Halo.component "LoadButton" (runAppM env)
  { initialState: \_ ->
      { fiber: Nothing, loading: false, result: Nothing }
  , handlers
  , onError: \context error ->
      Console.error $ showContext context <> ": " <> message error
  , render: \{ props, state, dispatch } ->
      R.div_
        [ R.text props.title
        , R.button
            { onClick: capture_ (dispatch Load)
            , children: [ R.text if state.loading then "Restart" else "Load" ]
            }
        , R.button
            { onClick: capture_ (dispatch Cancel)
            , children: [ R.text "Cancel" ]
            }
        ]
  }
```

`initialState` receives the initial props once per mount. Later prop changes call `handlers.onPropsChange`; they do not recreate state.

Use the hook form when Halo shares a component with other hooks:

```purescript
halo <- Halo.useHalo (runAppM env)
  { props
  , initialState
  , handlers
  , onError
  }

-- halo.state
-- halo.dispatch
```

## Learn and reference

- [Guide](docs/guide.md): application monads, handlers, component ownership, cancellation, parallelism, subscriptions, and errors.
- [API reference](docs/reference.md): public types and exact runtime semantics.

The documentation examples are compile-checked in `test/Test/Halo/DocExamples.purs`.

## Development

```console
npm ci
npm run format:check
npm run build -- --strict --pedantic-packages
npm test
npx spago docs
```

The deterministic runtime tests model React's setup-cleanup-setup sequence directly. A DOM mounting test is intentionally omitted because the package manifest contains only the pinned PureScript compiler and Spago; the hook uses the tested runtime boundary, and component examples are compile-checked.
