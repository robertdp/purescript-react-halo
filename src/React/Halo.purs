module React.Halo where

import Prelude
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (JSX)
import React.Basic.Hooks as React
import React.Halo.Component (Spec)
import React.Halo.Component.State (createInitialState)
import React.Halo.Eval (handleAction, handleUpdate, runFinalize, runInitialize)

component :: forall state action props. String -> Spec props state action Aff -> Effect (props -> JSX)
component name spec@{ init, render } =
  React.component name \props -> React.do
    state /\ setState <- React.useState' init
    halo <- React.useMemo unit \_ -> unsafePerformEffect (createInitialState spec setState props)
    React.useEffectOnce (runInitialize halo props *> pure (runFinalize halo))
    React.useEffectAlways (handleUpdate halo props *> mempty)
    pure (render { props, state, send: handleAction halo })

component_ :: forall state action. String -> Spec Unit state action Aff -> Effect JSX
component_ name spec = flap (component name spec) unit
