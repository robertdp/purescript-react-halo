module React.Halo.Component where

import Prelude
import Data.Newtype (class Newtype)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (Hook, JSX, UseEffect, UseMemo, UseState, Component)
import React.Basic.Hooks as React
import React.Halo.Internal.Control (HaloM)
import React.Halo.Internal.Eval (handleAction, handleUpdate, runFinalize, runInitialize)
import React.Halo.Internal.State (HaloState, createInitialState)
import React.Halo.Internal.Types (Lifecycle)

type HookSpec props state action m
  = { props :: props
    , initialState :: state
    , eval :: Lifecycle props action -> HaloM props state action m Unit
    }

newtype UseHalo props state action hooks
  = UseHalo (UseEffect Unit (UseEffect Unit (UseMemo Unit (HaloState props state action) (UseState state hooks))))

derive instance newtypeUseHalo :: Newtype (UseHalo props state action hooks) _

-- | Run renderless Halo in the current component. This allows Halo to be used with other hooks and other ways of
-- | building components.
useHalo ::
  forall state action props.
  HookSpec props state action Aff ->
  Hook (UseHalo props state action) (state /\ (action -> Effect Unit))
useHalo { props, initialState, eval } =
  React.coerceHook React.do
    state /\ setState <- React.useState' initialState
    halo <- React.useMemo unit \_ -> unsafePerformEffect (createInitialState { props, state: initialState, eval, update: setState })
    React.useEffectOnce (runInitialize halo *> pure (runFinalize halo))
    React.useEffectAlways (handleUpdate halo props *> mempty)
    pure (state /\ handleAction halo)

type ComponentSpec props state action m
  = { initialState :: state
    , eval :: Lifecycle props action -> HaloM props state action m Unit
    , render ::
        { props :: props
        , state :: state
        , send :: action -> Effect Unit
        } ->
        JSX
    }

-- | Build a component by providing a name and Halo component spec.
component :: forall state action props. String -> ComponentSpec props state action Aff -> Component props
component name { initialState, eval, render } =
  React.component name \props -> React.do
    state /\ send <- useHalo { props, initialState, eval }
    pure (render { props, state, send })
