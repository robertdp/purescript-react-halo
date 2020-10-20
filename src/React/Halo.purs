module React.Halo
  ( module Exports
  , component
  , component_
  ) where

import Prelude
import Control.Monad.Error.Class (throwError) as Exports
import Control.Monad.Reader (ask, asks) as Exports
import Control.Monad.State.Class (get, gets, modify, modify_, put, state) as Exports
import Control.Monad.Trans.Class (lift) as Exports
import Control.Monad.Writer (tell) as Exports
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff) as Exports
import Effect.Class (liftEffect) as Exports
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (JSX)
import React.Basic.Hooks as React
import React.Halo.Component (Lifecycle(..), Spec) as Exports
import React.Halo.Component (Spec)
import React.Halo.Component.Control (ForkId, HaloAp, HaloM, SubscriptionId, fork, hoist, kill, props, subscribe, subscribe', unsubscribe) as Exports
import React.Halo.Component.State (createInitialState)
import React.Halo.Eval (EvalSpec, defaultEval, makeEval) as Exports
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
