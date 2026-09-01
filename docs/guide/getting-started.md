# Getting started

Halo has two React entry points. Choose one first; their action, state, and application-effect model is otherwise the same.

## Use `component` for a complete component

`component` creates a React component from one Halo specification:

```purescript
loadButton :: Env -> Component Props
loadButton env =
  Halo.component "LoadButton" (runAppM env)
    { initialState: \_ -> initialState
    , handlers
    , onError
    , render
    }
```

The specification supplies:

- initial state derived from the initial props;
- action handlers;
- unexpected-error reporting; and
- a renderer receiving `{ props, state, tasks, dispatch }`.

Later prop changes do not recreate state. They start `onPropsChange` instead.

## Use `useHalo` with other hooks

`useHalo` fits the same runtime into a component that owns its render function:

```purescript
halo <- Halo.useHalo (runAppM env)
  { props
  , initialState
  , handlers
  , onError
  }

pure $ render
  { props
  , state: halo.state
  , tasks: halo.tasks
  , dispatch: halo.dispatch
  }
```

The hook returns current state, an immutable managed-task view, and synchronous dispatch. Use ordinary React hooks alongside it as needed.

## Define state and actions

State contains the values used for rendering. An action ADT describes events that rendering can send to Halo:

```purescript
type Props = { title :: String }

type State =
  { greeting :: Maybe String
  }

initialState :: State
initialState =
  { greeting: Nothing
  }

data Action = LoadGreeting
```

## Render state and dispatch actions

Rendering is ordinary `react-basic` code. Calling `dispatch` starts `handlers.onAction` in the active component scope:

```purescript
render { props, state, dispatch } =
  R.div_
    [ R.text props.title
    , R.button
        { onClick: capture_ (dispatch LoadGreeting)
        , children: [ R.text "Load" ]
        }
    , R.text $ fromMaybe "Not loaded" state.greeting
    ]
```

Dispatch is synchronous; the handler it starts may continue asynchronously.

## Handle the action

Start with `defaultHandlers` and replace only the callbacks the component needs:

```purescript
handlers :: Halo.Handlers Props State Action AppM
handlers = Halo.defaultHandlers
  { onAction = case _ of
      LoadGreeting -> do
        greeting <- lift loadGreeting
        modify_ _ { greeting = Just greeting }
  }
```

`HaloM` supplies component state and props. Standard transformer `lift` runs application logic through the interpreter passed to `component` or `useHalo`.

## Define the application boundary

Application services remain in the application's own monad:

```purescript
type Env =
  { loadGreeting :: Aff String
  }

newtype AppM a = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM program) = runReaderT program env

loadGreeting :: AppM String
loadGreeting = AppM do
  env <- ask
  liftAff env.loadGreeting
```

The component computation type makes both layers explicit:

```purescript
type UI a = Halo.HaloM Props State Action AppM a
```

The interpreter must return the `Aff` that performs the work. Do not detach it with `launchAff_`; Halo can own and cancel only the returned computation.

## Report unexpected errors

Both entry points require a synchronous error callback:

```purescript
onError _ error =
  Console.error $ message error
```

Expected domain failures should remain typed application values or component state. `onError` is for unexpected exceptions from a current Halo-owned root or synchronous cleanup.

Next, see [Actions, effects, and state](actions-and-state.md) for overlapping handlers, safe state updates, props changes, and parallel work.
