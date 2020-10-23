## Module React.Halo.Component

#### `Lifecycle`

``` purescript
data Lifecycle props action
  = Initialize props
  | Update props props
  | Action action
  | Finalize
```

#### `ComponentSpec`

``` purescript
type ComponentSpec props state action m = { eval :: Lifecycle props action -> HaloM props state action m Unit, initialState :: state, render :: { props :: props, send :: action -> Effect Unit, state :: state } -> JSX }
```

#### `HookSpec`

``` purescript
type HookSpec props state action m = { eval :: Lifecycle props action -> HaloM props state action m Unit, initialState :: state, props :: props }
```


