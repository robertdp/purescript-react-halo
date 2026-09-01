module Test.Halo.SubscriptionErrorSpec (spec) where

import Prelude

import Control.Monad.State (get, modify_, put)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff)
import Effect.AVar (AVar)
import Effect.AVar as EffectAVar
import Effect.Class (liftEffect)
import Effect.Exception as Exception
import Effect.Ref as Ref
import React.Halo.Handlers (Handlers, defaultHandlers)
import React.Halo.Internal.Runtime (Runtime, activate, createRuntime, deactivate, dispatch, fork, subscribe, syncSpec, unsubscribe)
import React.Halo.Internal.Types (ErrorContext(..), ForkId, SubscriptionId)
import React.Halo.Subscription (Emitter, makeEmitter)
import Test.Halo.Helpers (Gate, await, makeGate, release, waitForGate)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

identityAff :: Aff ~> Aff
identityAff = identity

spec :: Spec Unit
spec = describe "subscriptions and errors" do
  it "removes a manual unsubscribe from scope tracking" do
    cleanupCount <- liftEffect $ Ref.new 0
    started <- liftEffect EffectAVar.empty
    stopped <- liftEffect EffectAVar.empty
    state <- liftEffect $ Ref.new Nothing
    let emitter = makeEmitter \_ -> pure $ Ref.modify_ (_ + 1) cleanupCount

    runtime <- liftEffect $ createRuntime identityAff
      { initialProps: unit
      , initialState: Nothing
      , spec: { handlers: subscriptionHandlers, onError: \_ _ -> pure unit }
      , stateUpdate: flip Ref.write state
      }

    Aff.finally (liftEffect $ deactivate runtime) do
      liftEffect do
        activate runtime
        dispatch runtime (Start emitter started)
      void $ await "subscription setup" started
      liftEffect $ dispatch runtime (Stop stopped)
      void $ await "manual unsubscribe" stopped

      afterManual <- liftEffect $ Ref.read cleanupCount
      afterManual `shouldEqual` 1
      liftEffect $ deactivate runtime
      afterDeactivation <- liftEffect $ Ref.read cleanupCount
      afterDeactivation `shouldEqual` 1

  it "runs tracked subscription cleanup on deactivation" do
    cleanupCount <- liftEffect $ Ref.new 0
    started <- liftEffect EffectAVar.empty
    state <- liftEffect $ Ref.new Nothing
    let emitter = makeEmitter \_ -> pure $ Ref.modify_ (_ + 1) cleanupCount

    runtime <- liftEffect $ createRuntime identityAff
      { initialProps: unit
      , initialState: Nothing
      , spec: { handlers: subscriptionHandlers, onError: \_ _ -> pure unit }
      , stateUpdate: flip Ref.write state
      }

    Aff.finally (liftEffect $ deactivate runtime) do
      liftEffect do
        activate runtime
        dispatch runtime (Start emitter started)
      void $ await "tracked subscription setup" started
      liftEffect $ deactivate runtime
      cleaned <- liftEffect $ Ref.read cleanupCount
      cleaned `shouldEqual` 1

  it "isolates throwing cleanup and reports DeactivationError" do
    cleaned <- liftEffect $ Ref.new 0
    cleanupErrors <- liftEffect $ Ref.new []
    badStarted <- liftEffect EffectAVar.empty
    goodStarted <- liftEffect EffectAVar.empty
    state <- liftEffect $ Ref.new Nothing
    gate <- liftEffect makeGate
    let
      badEmitter = makeEmitter \_ -> pure $ Exception.throw "cleanup failed"
      goodEmitter = makeEmitter \_ -> pure $ Ref.modify_ (_ + 1) cleaned

    runtime <- liftEffect $ createRuntime identityAff
      { initialProps: unit
      , initialState: Nothing
      , spec:
          { handlers: subscriptionHandlers
          , onError: \context error -> case context of
              DeactivationError -> Ref.modify_ (_ <> [ Exception.message error ]) cleanupErrors
              _ -> Ref.modify_ (_ <> [ "wrong error context" ]) cleanupErrors
          }
      , stateUpdate: flip Ref.write state
      }

    liftEffect do
      activate runtime
      dispatch runtime (Start badEmitter badStarted)
    void $ await "failing subscription setup" badStarted
    liftEffect $ dispatch runtime (Start goodEmitter goodStarted)
    void $ await "successful subscription setup" goodStarted
    liftEffect $ dispatch runtime (Block gate)
    void $ await "running action handler" gate.started

    liftEffect $ deactivate runtime
    void $ await "running action cancellation" gate.settled

    cleanupCount <- liftEffect $ Ref.read cleaned
    cleanupCount `shouldEqual` 1
    errors <- liftEffect $ Ref.read cleanupErrors
    errors `shouldEqual` [ "cleanup failed" ]

  it "rejects a callback retained by a stale activation" do
    callback <- liftEffect $ Ref.new Nothing
    registered <- liftEffect EffectAVar.empty
    state <- liftEffect $ Ref.new 0
    let
      emitter = makeEmitter \receive -> do
        Ref.write (Just receive) callback
        pure (pure unit)
      handlers = defaultHandlers
        { onAction = case _ of
            Register completed -> do
              void $ subscribe emitter
              liftAff $ void $ AVar.tryPut unit completed
            Ping -> modify_ (_ + 1)
        }
    runtime <- liftEffect $
      ( createRuntime identityAff
          { initialProps: unit
          , initialState: 0
          , spec: { handlers, onError: \_ _ -> pure unit }
          , stateUpdate: flip Ref.write state
          } :: Effect (Runtime Unit Int StaleAction Aff)
      )

    liftEffect do
      activate runtime
      dispatch runtime (Register registered)
    void $ await "stale callback registration" registered
    retained <- liftEffect $ Ref.read callback
    liftEffect do
      deactivate runtime
      activate runtime
      traverse_ (\receive -> receive Ping) retained
    value <- liftEffect $ Ref.read state
    value `shouldEqual` 0
    liftEffect $ deactivate runtime

  it "routes action failures and uses the latest onError callback" do
    gate <- liftEffect makeGate
    oldErrors <- liftEffect $ Ref.new []
    newErrors <- liftEffect $ Ref.new []
    newRaised <- liftEffect EffectAVar.empty
    runtime <- liftEffect $ makeErrorRuntime oldErrors

    Aff.finally (liftEffect $ deactivate runtime) do
      liftEffect do
        activate runtime
        dispatch runtime (Boom gate)
      void $ await "failing action start" gate.started

      liftEffect $ syncSpec runtime identityAff
        { spec:
            { handlers: errorHandlers
            , onError: \context error -> do
                let
                  label = case context of
                    ActionError (Boom _) -> "action"
                    _ -> "wrong context"
                Ref.modify_ (_ <> [ label <> ": " <> Exception.message error ]) newErrors
                void $ EffectAVar.tryPut unit newRaised
            }
        , stateUpdate: \_ -> pure unit
        }
      release gate
      void $ await "latest action error callback" newRaised

      previous <- liftEffect $ Ref.read oldErrors
      current <- liftEffect $ Ref.read newErrors
      previous `shouldEqual` []
      current `shouldEqual` [ "action: action boom" ]

  it "routes an unexpected fork failure with ForkError" do
    gate <- liftEffect makeGate
    forkId <- liftEffect EffectAVar.empty
    errors <- liftEffect $ Ref.new []
    raised <- liftEffect EffectAVar.empty
    runtime <- liftEffect $
      ( createRuntime identityAff
          { initialProps: unit
          , initialState: unit
          , spec:
              { handlers: errorHandlers
              , onError: \context error -> do
                  let
                    label = case context of
                      ForkError fid -> "fork " <> show fid
                      _ -> "wrong context"
                  Ref.modify_ (_ <> [ label <> ": " <> Exception.message error ]) errors
                  void $ EffectAVar.tryPut unit raised
              }
          , stateUpdate: \_ -> pure unit
          } :: Effect (Runtime Unit Unit ErrorAction Aff)
      )

    Aff.finally (liftEffect $ deactivate runtime) do
      liftEffect do
        activate runtime
        dispatch runtime (ForkBoom gate forkId)
      fid <- await "failing fork id" forkId
      void $ await "failing fork start" gate.started
      release gate
      void $ await "fork error callback" raised
      actual <- liftEffect $ Ref.read errors
      actual `shouldEqual` [ "fork " <> show fid <> ": fork boom" ]

data SubscriptionAction
  = Start (Emitter SubscriptionAction) (AVar Unit)
  | Stop (AVar Unit)
  | Block Gate

type SubscriptionState = Maybe SubscriptionId

subscriptionHandlers :: Handlers Unit SubscriptionState SubscriptionAction Aff
subscriptionHandlers = defaultHandlers
  { onAction = case _ of
      Start emitter completed -> do
        sid <- subscribe emitter
        put (Just sid)
        liftAff $ void $ AVar.tryPut unit completed
      Stop completed -> do
        sid <- get
        traverse_ unsubscribe sid
        put Nothing
        liftAff $ void $ AVar.tryPut unit completed
      Block gate -> liftAff $ waitForGate gate
  }

data StaleAction
  = Register (AVar Unit)
  | Ping

data ErrorAction
  = Boom Gate
  | ForkBoom Gate (AVar ForkId)

errorHandlers :: Handlers Unit Unit ErrorAction Aff
errorHandlers = defaultHandlers
  { onAction = case _ of
      Boom gate -> do
        liftAff $ waitForGate gate
        liftAff $ Aff.throwError (Aff.error "action boom")
      ForkBoom gate fid -> do
        child <- fork do
          liftAff $ waitForGate gate
          liftAff $ Aff.throwError (Aff.error "fork boom")
        liftAff $ void $ AVar.tryPut child fid
  }

makeErrorRuntime :: Ref.Ref (Array String) -> Effect (Runtime Unit Unit ErrorAction Aff)
makeErrorRuntime errors = createRuntime identityAff
  { initialProps: unit
  , initialState: unit
  , spec:
      { handlers: errorHandlers
      , onError: \_ error -> Ref.modify_ (_ <> [ Exception.message error ]) errors
      }
  , stateUpdate: \_ -> pure unit
  }
