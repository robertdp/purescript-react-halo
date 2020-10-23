## Module React.Halo.Eval

#### `evalHaloM`

``` purescript
evalHaloM :: forall props state action. HaloState props state action -> (HaloM props state action Aff) ~> Aff
```

#### `evalHaloF`

``` purescript
evalHaloF :: forall props state action. HaloState props state action -> (HaloF props state action Aff) ~> Aff
```

#### `EvalSpec`

``` purescript
type EvalSpec props action state m = { onAction :: action -> HaloM props state action m Unit, onFinalize :: Maybe action, onInitialize :: props -> Maybe action, onUpdate :: props -> props -> Maybe action }
```

#### `defaultEval`

``` purescript
defaultEval :: forall props action state m. EvalSpec props action state m
```

#### `makeEval`

``` purescript
makeEval :: forall props action state m. EvalSpec props action state m -> Lifecycle props action -> HaloM props state action m Unit
```

#### `runAff`

``` purescript
runAff :: Aff Unit -> Effect Unit
```

#### `runInitialize`

``` purescript
runInitialize :: forall props state action. HaloState props action state -> props -> Effect Unit
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


