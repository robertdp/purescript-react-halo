module React.Halo.Component where

import Prelude
import Data.Newtype (class Newtype)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (Component, Hook, JSX, Render, UseEffect, UseMemo, UseState)
import React.Basic.Hooks as React
import React.Halo.Internal.Control (HaloM)
import React.Halo.Internal.Eval (handleAction, handleUpdate, runFinalize, runInitialize)
import React.Halo.Internal.State (HaloState, createInitialState)
import React.Halo.Internal.Types (Lifecycle)

type HookSpec props context state action m
  = { props :: props
    , context :: context
    , initialState :: state
    , eval :: Lifecycle props context action -> HaloM props context state action m Unit
    }

newtype UseHalo props context state action hooks
  = UseHalo (UseEffect Unit (UseEffect Unit (UseMemo Unit (HaloState props context state action) (UseState state hooks))))

derive instance newtypeUseHalo :: Newtype (UseHalo props context state action hooks) _

-- | Run renderless Halo in the current component. This allows Halo to be used with other hooks and other ways of
-- | building components.
useHalo ::
  forall props context state action.
  HookSpec props context state action Aff ->
  Hook (UseHalo props context state action) (state /\ (action -> Effect Unit))
useHalo { props, context, initialState, eval } =
  React.coerceHook React.do
    state /\ setState <- React.useState' initialState
    halo <-
      React.useMemo unit \_ ->
        unsafePerformEffect do
          createInitialState { props, context, state: initialState, eval, update: setState }
    React.useEffectOnce (runInitialize halo *> pure (runFinalize halo))
    React.useEffectAlways (handleUpdate halo props context *> mempty)
    pure (state /\ handleAction halo)

type ComponentSpec props context state action m
  = { hooks :: forall hooks. Render Unit hooks context
    , initialState :: props -> context -> state
    , eval :: Lifecycle props context action -> HaloM props context state action m Unit
    , render ::
        { props :: props
        , context :: context
        , state :: state
        , send :: action -> Effect Unit
        } ->
        JSX
    }

-- | Build a component by providing a name and a Halo component spec.
component ::
  forall props context state action.
  String ->
  ComponentSpec props context state action Aff ->
  Component props
component name spec@{ eval, render } =
  React.component name \props -> React.do
    context <- spec.hooks
    initialState <- React.useMemo unit \_ -> spec.initialState props context
    state /\ send <- useHalo { props, context, initialState, eval }
    pure (render { props, context, state, send })
