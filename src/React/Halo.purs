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
import Data.Newtype (class Newtype)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff) as Exports
import Effect.Class (liftEffect) as Exports
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (JSX, UseEffect, UseMemo, UseState, Hook)
import React.Basic.Hooks as React
import React.Halo.Component (ComponentSpec, HookSpec)
import React.Halo.Component (Lifecycle(..), ComponentSpec, HookSpec) as Exports
import React.Halo.Component.Control (ForkId, HaloAp, HaloM, SubscriptionId, fork, hoist, kill, props, subscribe, subscribe', unsubscribe) as Exports
import React.Halo.Component.State (HaloState, createInitialState)
import React.Halo.Eval (EvalSpec, defaultEval, makeEval) as Exports
import React.Halo.Eval (handleAction, handleUpdate, runFinalize, runInitialize)

component :: forall state action props. String -> ComponentSpec props state action Aff -> Effect (props -> JSX)
component name { initialState, eval, render } =
  React.component name \props -> React.do
    state /\ send <- useHalo { props, initialState, eval }
    pure (render { props, state, send })

component_ :: forall state action. String -> ComponentSpec Unit state action Aff -> Effect JSX
component_ name spec = flap (component name spec) unit

newtype UseHalo props state action hooks
  = UseHalo (UseEffect Unit (UseEffect Unit (UseMemo Unit (HaloState props state action) (UseState state hooks))))

derive instance newtypeUseHalo :: Newtype (UseHalo props state action hooks) _

useHalo ::
  forall state action props.
  HookSpec props state action Aff ->
  Hook (UseHalo props state action) (state /\ (action -> Effect Unit))
useHalo { props, initialState, eval } =
  React.coerceHook React.do
    state /\ setState <- React.useState' initialState
    halo <- React.useMemo unit \_ -> unsafePerformEffect (createInitialState initialState eval setState props)
    React.useEffectOnce (runInitialize halo props *> pure (runFinalize halo))
    React.useEffectAlways (handleUpdate halo props *> mempty)
    pure (state /\ handleAction halo)
