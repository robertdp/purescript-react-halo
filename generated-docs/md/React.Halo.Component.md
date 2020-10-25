## Module React.Halo.Component

#### `HookSpec`

``` purescript
type HookSpec props state action m = { eval :: Lifecycle props action -> HaloM props state action m Unit, initialState :: state, props :: props }
```

#### `UseHalo`

``` purescript
newtype UseHalo props state action hooks
  = UseHalo (UseEffect Unit (UseEffect Unit (UseMemo Unit (HaloState props state action) (UseState state hooks))))
```

##### Instances
``` purescript
Newtype (UseHalo props state action hooks) _
```

#### `useHalo`

``` purescript
useHalo :: forall state action props. HookSpec props state action Aff -> Hook (UseHalo props state action) (state /\ (action -> Effect Unit))
```

Run renderless Halo in the current component. This allows Halo to be used with other hooks and other ways of
building components.

#### `ComponentSpec`

``` purescript
type ComponentSpec props state action m = { eval :: Lifecycle props action -> HaloM props state action m Unit, initialState :: state, render :: { props :: props, send :: action -> Effect Unit, state :: state } -> JSX }
```

#### `component`

``` purescript
component :: forall state action props. String -> ComponentSpec props state action Aff -> Effect (props -> JSX)
```

Build a component by providing a name and Halo component spec.

#### `component_`

``` purescript
component_ :: forall state action. String -> ComponentSpec Unit state action Aff -> Effect JSX
```

Build a propless component by providing a name and Halo component spec.


