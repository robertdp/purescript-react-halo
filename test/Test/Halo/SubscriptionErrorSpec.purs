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
import Effect.Ref as Ref
import Halogen.Subscription (Emitter)
import Halogen.Subscription as HS
import React.Halo.Internal.Runtime (HaloM, activate, createRuntime, deactivate, dispatch, subscribe, unsubscribe)
import React.Halo.Internal.Types (Lifecycle(..), SubscriptionId, TaskPolicy(..), activityTotals, emptyActivity)
import Test.Halo.Helpers (Action(..), await, awaitCounts, makeGate, withHarness)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "subscriptions and errors" do
  it "removes a manually unsubscribed resource from component tracking" do
    cleanupCount <- liftEffect $ Ref.new 0
    callback <- liftEffect $ Ref.new Nothing
    started <- liftEffect EffectAVar.empty
    stopped <- liftEffect EffectAVar.empty
    state <- liftEffect $ Ref.new Nothing
    activity <- liftEffect $ Ref.new emptyActivity
    let
      emitter = HS.makeEmitter \receive -> do
        Ref.write (Just receive) callback
        pure $ Ref.modify_ (_ + 1) cleanupCount

    runtime <- liftEffect $ createRuntime
      { activityUpdate: flip Ref.write activity
      , initialProps: unit
      , initialState: Nothing
      , spec:
          { eval: subscriptionEval
          , onError: \_ _ -> pure unit
          , schedule: \_ -> Every
          }
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
      liftEffect do
        deactivate runtime
        activate runtime
      afterDeactivation <- liftEffect $ Ref.read cleanupCount
      afterDeactivation `shouldEqual` 1

      -- Even if a broken source invokes its retained callback after cleanup,
      -- that callback is bound to the old scope and cannot target reactivation.
      retained <- liftEffect $ Ref.read callback
      liftEffect $ traverse_ (_ $ Ping) retained
      counts <- activityTotals <$> liftEffect (Ref.read activity)
      counts `shouldEqual` { running: 0, queued: 0 }

  it "unsubscribes tracked resources on deactivation" do
    cleanupCount <- liftEffect $ Ref.new 0
    started <- liftEffect EffectAVar.empty
    state <- liftEffect $ Ref.new Nothing
    let emitter = HS.makeEmitter \_ -> pure $ Ref.modify_ (_ + 1) cleanupCount

    runtime <- liftEffect $ createRuntime
      { activityUpdate: \_ -> pure unit
      , initialProps: unit
      , initialState: Nothing
      , spec:
          { eval: subscriptionEval
          , onError: \_ _ -> pure unit
          , schedule: \_ -> Every
          }
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

  it "routes unexpected action failures with action context" $ withHarness \harness -> do
    gate <- liftEffect makeGate
    liftEffect $ dispatch harness.runtime (Boom gate)
    void $ await "spec-level error handler" harness.errorRaised
    awaitCounts harness { running: 0, queued: 0 }

    errors <- liftEffect $ Ref.read harness.errors
    errors `shouldEqual` [ "action: boom" ]

data SubscriptionAction
  = Start (Emitter SubscriptionAction) (AVar Unit)
  | Stop (AVar Unit)
  | Ping

subscriptionEval
  :: Lifecycle Unit SubscriptionAction
  -> HaloM Unit (Maybe SubscriptionId) SubscriptionAction Unit Unit
subscriptionEval = case _ of
  Activate -> pure unit
  Update _ -> pure unit
  Action (Start emitter completed) -> do
    sid <- subscribe emitter
    put (Just sid)
    liftAff $ void $ AVar.tryPut unit completed
  Action (Stop completed) -> do
    sid <- get
    traverse_ unsubscribe sid
    put Nothing
    liftAff $ void $ AVar.tryPut unit completed
  Action Ping -> pure unit
