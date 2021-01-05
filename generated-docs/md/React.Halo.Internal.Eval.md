## Module React.Halo.Internal.Eval

#### `evalHaloM`

``` purescript
evalHaloM :: forall props state action. HaloState props state action -> (HaloM props state action Aff) ~> Aff
```

Interprets `HaloM` into the base monad `Aff` for asynchronous effects.

#### `evalHaloAp`

``` purescript
evalHaloAp :: forall props state action. HaloState props state action -> (HaloAp props state action Aff) ~> ParAff
```

Interprets `HaloAp` into the base applicative `ParAff` for parallel effects.

#### `evalHaloF`

``` purescript
evalHaloF :: forall props state action. HaloState props state action -> (HaloF props state action Aff) ~> Aff
```

Interprets `HaloF` into the base monad `Aff`, keeping track of state in `HaloState`.

#### `EvalSpec`

``` purescript
type EvalSpec props state action m = { onAction :: action -> HaloM props state action m Unit, onFinalize :: Maybe action, onInitialize :: props -> Maybe action, onUpdate :: props -> props -> Maybe action }
```

A simpler interface for building the components eval function. The main lifecycle events map directly into
actions, so only the action handling logic needs to be written using `HaloM`.

#### `defaultEval`

``` purescript
defaultEval :: forall props action state m. EvalSpec props state action m
```

The empty `EvalSpec`.

#### `makeEval`

``` purescript
makeEval :: forall m action state props. (EvalSpec props state action m -> EvalSpec props state action m) -> Lifecycle props action -> HaloM props state action m Unit
```

Given an `EvalSpec` builder, it will return an eval function.

#### `runAff`

``` purescript
runAff :: Aff Unit -> Effect Unit
```

Simple way to run Aff logic asynchronously, while bringing errors back into Effect.

#### `runInitialize`

``` purescript
runInitialize :: forall props state action. HaloState props action state -> Effect Unit
```

#### `handleUpdate`

``` purescript
handleUpdate :: forall props state action. HaloState props action state -> props -> Effect Unit
```

#### `handleAction`

``` purescript
handleAction :: forall props state action. HaloState props state action -> action -> Effect Unit
```

#### `runFinalize`

``` purescript
runFinalize :: forall props state action. HaloState props state action -> Effect Unit
```


