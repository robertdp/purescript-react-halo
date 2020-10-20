module React.Halo where

import Prelude
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (JSX)
import React.Basic.Hooks as React
import React.Halo.Component (Spec)
import React.Halo.Component.State as State
import React.Halo.Eval as Eval

component ::
  forall state action props.
  String ->
  Spec props state action Aff ->
  Effect (props -> JSX)
component name spec@{ init, render } =
  React.component name \props -> React.do
    state /\ setState <- React.useState' init
    halo <-
      React.useMemo unit \_ ->
        unsafePerformEffect do
          State.createInitialState spec setState props
    React.useEffectOnce do
      Eval.runInitialize halo props
      pure do
        Eval.runFinalize halo
    React.useEffectAlways do
      Eval.handleUpdate halo props
      mempty
    pure (render { props, state, send: Eval.handleAction halo })
