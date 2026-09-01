# Actions, effects, and state

A dispatched action starts an independent handler in the current React activation. Handlers can read current state and props, update state, and lift application logic.

## Dispatch a typed action

Keep UI events in an action ADT rather than calling asynchronous logic from rendering:

```purescript
data Action
  = NameChanged String
  | Save

render { dispatch } =
  R.input
    { onChange: handler targetValue (dispatch <<< NameChanged)
    , value: ""
    }
```

Each call to `dispatch` starts `onAction` synchronously. Dispatch while the component is inactive is ignored.

## Handle actions independently

Use `defaultHandlers` when only some lifecycle callbacks matter:

```purescript
handlers = Halo.defaultHandlers
  { onAction = case _ of
      NameChanged name ->
        modify_ _ { name = name }

      Save ->
        saveCurrentForm
  }
```

Handlers overlap. A long-running `Save` does not block a later `NameChanged`. If later actions must cancel or supersede work, use a [managed task or fork](managed-work.md) rather than relying on handler ordering.

## Lift application effects

Application logic stays in AppM and enters Halo through standard transformer `lift`:

```purescript
saveCurrentForm = do
  form <- gets _.form
  result <- lift (Form.save form)
  modify_ _ { saveResult = Just result }
```

Every handler captures the interpreter current when it starts. A later React render may supply a newer interpreter for new handlers, but it does not change one already running.

## Update current state

`HaloM` has `MonadState state`, so normal state operations are available:

```purescript
name <- gets _.name
modify_ _ { submittedName = Just name }
```

State operations use the state current at that operation. Avoid restoring a whole snapshot after asynchronous work:

```purescript
-- Avoid: this can overwrite a newer action's changes.
old <- get
result <- lift (save old.form)
put (old { result = Just result })
```

Capture only the effect input, then update current state:

```purescript
form <- gets _.form
result <- lift (save form)
modify_ _ { result = Just result }
```

This is especially important because action handlers, tasks, and forks may overlap.

## Read current and previous props

`onPropsChange` receives the previous props. `getProps` reads the current props:

```purescript
handlers = Halo.defaultHandlers
  { onPropsChange = \previous -> do
      current <- Halo.getProps
      synchronize previous current
  }
```

Capture props before waiting when the work must use one render's value. Otherwise, a later `getProps` intentionally observes newer props.

## Run independent effects in parallel

`HaloM` has a direct `Parallel` instance. Run independent application reads in parallel, then update component state once:

```purescript
loadDashboard = do
  Tuple profile feed <- sequential ado
    profile <- parallel (lift Profile.load)
    feed <- parallel (lift Feed.load)
    in Tuple profile feed

  modify_ _ { profile = profile, feed = feed }
```

Parallel branches share one root, activation, error context, and interpreter snapshot. Concurrent component-state writes have nondeterministic ordering, so prefer combining results before one update.

Parallel work is lexical: the surrounding computation waits for every branch. Use a [fork](managed-work.md#use-a-fork-for-an-independent-process) when work must continue beyond its launching handler.
