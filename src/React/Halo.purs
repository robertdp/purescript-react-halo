module React.Halo where

import Prelude
import Control.Monad.Free (liftF)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.AVar as AffVar
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (JSX)
import React.Basic.Hooks as React
import React.Halo.Component (Lifecycle(..))
import React.Halo.Component.Control (HaloM(..))
import React.Halo.Eval (evalHaloM)
import Unsafe.Reference (unsafeRefEq)

type Component props state action
  = { init :: state
    , eval :: Lifecycle props action -> HaloM props state action Aff Unit
    , render ::
        { props :: props
        , state :: state
        , dispatch :: action -> Effect Unit
        } ->
        JSX
    }

component ::
  forall state action props.
  String ->
  Component props state action ->
  Effect (props -> JSX)
component name { init, eval, render } =
  React.component name \props -> React.do
    { eventQueue, propsRef, stateRef } <-
      React.useMemo unit \_ ->
        unsafePerformEffect ado
          eventQueue <- AVar.empty
          propsRef <- Ref.new props
          stateRef <- Ref.new init
          in { eventQueue, propsRef, stateRef }
    store /\ modifyStore <-
      React.useState
        { dispatch: \action -> Aff.launchAff_ do AffVar.put (Action action) eventQueue
        , state: init
        }
    React.useEffectAlways do
      props' <- Ref.read propsRef
      unless (unsafeRefEq props props') do
        Ref.write props propsRef
        Aff.launchAff_ $ AffVar.put (Props props props') eventQueue
      mempty
    React.useEffectOnce do
      let
        runStore event = do
          evalHaloM
            { stateRef
            , render: \state -> modifyStore _ { state = state }
            , enqueueAction: store.dispatch
            }
            (eval event)
      blockUntilUnmount <- AVar.empty
      (Aff.launchAff_ <<< Aff.supervise) do
        runStore (Initialize props)
        fiber <-
          forever do
            event <- lift $ AffVar.take eventQueue
            Aff.forkAff $ runStore event
        lift do
          AffVar.take blockUntilUnmount
          Aff.killFiber (Aff.error "Finalizing") fiber
          AffVar.kill (Aff.error "Finalizing") blockUntilUnmount
        runStore Finalize
      pure do
        Aff.launchAff_ do
          AffVar.kill (Aff.error "Finalizing") eventQueue
          AffVar.put unit blockUntilUnmount
    pure (render { props, state: store.state, dispatch: store.dispatch })
