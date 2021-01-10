## Module React.Halo.Internal.State

#### `HaloState`

``` purescript
newtype HaloState props state action
  = HaloState { eval :: Lifecycle props action -> HaloM props state action Aff Unit, finalized :: Ref Boolean, forks :: Ref (Map ForkId (Fiber Unit)), fresh :: Ref Int, props :: Ref props, state :: Ref state, subscriptions :: Ref (Map SubscriptionId (Effect Unit)), update :: state -> Effect Unit }
```

HThe alo component state used during evaluation.

#### `createInitialState`

``` purescript
createInitialState :: forall props state action. { eval :: Lifecycle props action -> HaloM props state action Aff Unit, props :: props, state :: state, update :: state -> Effect Unit } -> Effect (HaloState props state action)
```

Creates a starting `HaloState`, ready for initialization.

#### `fresh`

``` purescript
fresh :: forall props state action a. (Int -> a) -> HaloState props state action -> Effect a
```

Issue a new identifier, unique to this component.


