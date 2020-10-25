## Module React.Halo.Internal.State

#### `HaloState`

``` purescript
newtype HaloState props state action
  = HaloState { eval :: Lifecycle props action -> HaloM props state action Aff Unit, forks :: Ref (Map ForkId (Fiber Unit)), fresh :: Ref Int, props :: Ref props, render :: state -> Effect Unit, state :: Ref state, subscriptions :: Ref (Map SubscriptionId (Effect Unit)), unmounted :: Ref Boolean }
```

HThe alo component state used during evaluation.

#### `createInitialState`

``` purescript
createInitialState :: forall props state action. state -> (Lifecycle props action -> HaloM props state action Aff Unit) -> (state -> Effect Unit) -> props -> Effect (HaloState props state action)
```

Creates a starting `HaloState`, ready for initialization.

#### `fresh`

``` purescript
fresh :: forall props state action a. (Int -> a) -> HaloState props state action -> Effect a
```

Issue a new identifier, unique to this component.


