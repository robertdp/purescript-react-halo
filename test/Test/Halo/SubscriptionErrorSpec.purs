module Test.Halo.SubscriptionErrorSpec (spec) where

import Prelude

import Control.Monad.State (get, put)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect.Aff as Aff
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff)
import Effect.AVar (AVar)
import Effect.AVar as EffectAVar
import Effect.Class (liftEffect)
import Effect.Exception as Exception
import Effect.Ref as Ref
import React.Halo.Handlers (Handlers, defaultHandlers)
import React.Halo.Internal.Runtime (activate, createRuntime, deactivate, dispatch, subscribe, syncSpec, unsubscribe)
import React.Halo.Internal.Types (ErrorContext(..), SubscriptionId, TaskPolicy(..))
import React.Halo.Subscription (Emitter, makeEmitter)
import Test.Halo.Helpers (Action(..), Gate, Key(..), await, awaitCounts, handlers, makeGate, withHarness)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "subscriptions and errors" do
  it "removes a manual unsubscribe from scope tracking" do
    cleanupCount <- liftEffect $ Ref.new 0
    started <- liftEffect EffectAVar.empty
    stopped <- liftEffect EffectAVar.empty
    state <- liftEffect $ Ref.new Nothing
    let emitter = makeEmitter \_ -> pure $ Ref.modify_ (_ + 1) cleanupCount

    runtime <- liftEffect $ createRuntime
      { activityUpdate: \_ -> pure unit
      , initialProps: unit
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

    runtime <- liftEffect $ createRuntime
      { activityUpdate: \_ -> pure unit
      , initialProps: unit
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

    runtime <- liftEffect $ createRuntime
      { activityUpdate: \_ -> pure unit
      , initialProps: unit
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

  it "routes an unexpected action failure with ActionError" $ withHarness \harness -> do
    gate <- liftEffect makeGate
    liftEffect $ dispatch harness.runtime (Boom gate)
    void $ await "action error handler" harness.errorRaised

    errors <- liftEffect $ Ref.read harness.errors
    errors `shouldEqual` [ "action: boom" ]

  it "uses the latest unexpected-error callback after a spec change" $ withHarness \harness -> do
    replacementErrors <- liftEffect $ Ref.new []
    replacementRaised <- liftEffect EffectAVar.empty
    gate <- liftEffect makeGate
    liftEffect do
      syncSpec harness.runtime
        { activityUpdate: \next -> do
            Ref.write next harness.activity
            void $ EffectAVar.tryPut unit harness.activityChanged
        , spec:
            { handlers
            , onError: \context error -> do
                let
                  label = case context of
                    ActionError _ -> "replacement action"
                    _ -> "wrong replacement context"
                Ref.modify_ (_ <> [ label <> ": " <> Exception.message error ]) replacementErrors
                void $ EffectAVar.tryPut unit replacementRaised
            }
        , stateUpdate: flip Ref.write harness.state
        }
      dispatch harness.runtime (Boom gate)

    void $ await "replacement error callback" replacementRaised
    oldErrors <- liftEffect $ Ref.read harness.errors
    oldErrors `shouldEqual` []
    newErrors <- liftEffect $ Ref.read replacementErrors
    newErrors `shouldEqual` [ "replacement action: boom" ]

  it "routes an explicit task failure with TaskError" $ withHarness \harness -> do
    gate <- liftEffect makeGate
    liftEffect $ dispatch harness.runtime (TaskBoom (Restartable Save) gate)
    void $ await "failing task start" gate.started
    void $ await "task error handler" harness.errorRaised
    awaitCounts harness { running: 0, queued: 0 }

    errors <- liftEffect $ Ref.read harness.errors
    errors `shouldEqual` [ "task: task boom" ]

data SubscriptionAction
  = Start (Emitter SubscriptionAction) (AVar Unit)
  | Stop (AVar Unit)
  | Block Gate

type SubscriptionState = Maybe SubscriptionId

subscriptionHandlers :: Handlers Unit SubscriptionState SubscriptionAction Unit
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
      Block gate ->
        liftAff $ Aff.finally
          (void $ AVar.tryPut unit gate.settled)
          do
            AVar.put unit gate.started
            void $ AVar.take gate.release
  }
