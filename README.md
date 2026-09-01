# React Halo

Halo gives a PureScript React component a typed action handler, local state, and a safe boundary for application effects. Your application logic remains in its own monad; Halo adds access to props and state, action dispatch, component-owned processes, subscriptions, and cleanup.

Use Halo when several UI interactions share state and asynchronous work must remain owned by the component. For a single request derived directly from render dependencies, `React.Basic.Hooks.Aff.useAff` is usually simpler.

## How Halo fits

A Halo component has three main parts:

1. **Actions** describe UI interactions. Rendering code calls `dispatch :: action -> Effect Unit`, and Halo starts the corresponding action handler in the active component scope.
2. **Application effects** remain in an application monad such as `ReaderT Env Aff`. Standard `lift` embeds those effects in `HaloM`, and an interpreter supplied at the React boundary translates them to `Aff`.
3. **Forks** are cancellable processes owned by the active component. A fork may outlive the handler that started it, but it cannot outlive the React activation that owns it.

Halo does not provide global state, server caching, or a separate process runtime.

## Install this unreleased version

The API documented on this branch is not published yet; the PureScript Registry currently resolves `react-halo` to v3. This branch uses the PureScript and Spago versions pinned in [`package.json`](package.json).

Add a checkout as a local Spago package and declare the dependencies imported by the example below:

```yaml
package:
  dependencies:
    - aff
    - console
    - effect
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

After v4 is published, the local override can be replaced with:

```console
spago install aff console effect exceptions foldable-traversable maybe prelude react-basic-dom react-basic-hooks react-halo transformers
```

`react-basic-dom` is used by this example, not required by Halo itself. Your application also needs the JavaScript packages required by `react-basic-hooks`, including React. Halo has no npm runtime entry point or npm runtime dependencies.

## Quick start

Define an application monad and the interpreter that runs it:

```purescript
type Env = { loadGreeting :: Aff String }

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

loadGreeting :: AppM String
loadGreeting = AppM do
  env <- ask
  liftAff env.loadGreeting
```

Define component state and an action ADT. Store a `ForkId` when another action must be able to cancel the process:

```purescript
type Props = { title :: String }

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

`lift` is `Control.Monad.Trans.Class.lift`. Each handler captures the interpreter current when it starts. A fork inherits its launching handler's interpreter, even if React renders with a newer interpreter before the fork begins.

Supply the interpreter when creating the component:

```purescript
loadButton :: Env -> Component Props
loadButton env = Halo.component "LoadButton" (runAppM env)
  { initialState: \_ ->
      { fiber: Nothing, loading: false, result: Nothing }
  , handlers
  , onError: \_ error ->
      Console.error $ "Unexpected Halo error: " <> message error
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
        , R.text $ case state.result of
            Nothing -> if state.loading then "Loading…" else "Not loaded"
            Just greeting -> greeting
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

A complete version of this example is compiled as [`test/Test/Halo/DocExamples.purs`](test/Test/Halo/DocExamples.purs).

## Learn more

- The [Halo guide](docs/guide.md) explains actions, state, component processes, cancellation, parallelism, subscriptions, and errors.
- Generate the exact API reference from public source comments with `npx spago docs --offline`.
- The [runtime architecture](docs/architecture.md) describes ownership and cancellation invariants for maintainers.
- See [Contributing](CONTRIBUTING.md) before changing the library.

The deterministic tests model React's setup-cleanup-setup sequence directly. The repository does not yet include a real DOM/StrictMode mounting fixture.
