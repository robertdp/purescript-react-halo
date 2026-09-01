module Test.Halo.LifecycleSpec (spec) where

import Prelude

import Control.Monad.State (modify_)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff)
import Effect.AVar (AVar)
import Effect.AVar as EffectAVar
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import React.Halo.Internal.Runtime (HaloM, Runtime, activate, createRuntime, deactivate, dispatch, fork, runForTest, syncSpec, updateProps)
import React.Halo.Internal.Types (ErrorContext(..), Lifecycle(..), TaskPolicy(..))
import Test.Halo.Helpers (Action(..), Gate, Key(..), await, awaitCounts, makeGate, policyOf, release, shouldNotHaveStarted, withHarness)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "scope lifecycle" do
  it "cancels running and queued work, clears activity, and fences commits on deactivation" $ withHarness \harness -> do
    running <- liftEffect makeGate
    queued <- liftEffect makeGate
    ignored <- liftEffect makeGate

    liftEffect do
      dispatch harness.runtime (Work (Enqueue Save) 1 running)
      dispatch harness.runtime (Work (Enqueue Save) 2 queued)
    void $ await "running action start before deactivation" running.started
    awaitCounts harness { running: 1, queued: 1 }

    liftEffect $ deactivate harness.runtime
    void $ await "running action cancellation on deactivation" running.settled
    awaitCounts harness { running: 0, queued: 0 }
    shouldNotHaveStarted queued

    liftEffect $ dispatch harness.runtime (Work Every 3 ignored)
    shouldNotHaveStarted ignored
    state <- liftEffect $ Ref.read harness.state
    state `shouldEqual` []

    reactivated <- liftEffect makeGate
    liftEffect do
      activate harness.runtime
      dispatch harness.runtime (Work Every 4 reactivated)
    void $ await "action start after reactivation" reactivated.started
    release reactivated
    void $ await "action completion after reactivation" reactivated.settled
    awaitCounts harness { running: 0, queued: 0 }
    reactivatedState <- liftEffect $ Ref.read harness.state
    reactivatedState `shouldEqual` [ 4 ]

  it "models StrictMode setup-cleanup-setup with a fresh usable active scope" do
    activation <- liftEffect EffectAVar.empty
    state <- liftEffect $ Ref.new 0
    runtime <- liftEffect $ createRuntime
      { activityUpdate: \_ -> pure unit
      , initialProps: unit
      , initialState: 0
      , spec:
          { eval: replayEval activation
          , onError: \_ _ -> pure unit
          , schedule: \_ -> Every
          }
      , stateUpdate: flip Ref.write state
      }

    Aff.finally (liftEffect $ deactivate runtime) do
      liftEffect $ activate runtime
      void $ await "first activation" activation
      first <- liftEffect $ Ref.read state
      first `shouldEqual` 1

      liftEffect do
        deactivate runtime
        activate runtime
      void $ await "StrictMode replay activation" activation

      pulse <- liftEffect EffectAVar.empty
      liftEffect $ dispatch runtime (Pulse pulse)
      void $ await "action after StrictMode replay" pulse
      second <- liftEffect $ Ref.read state
      second `shouldEqual` 12

  it "owns and cancels prop-update evaluations" do
    gate <- liftEffect makeGate
    state <- liftEffect $ Ref.new 0
    runtime <- liftEffect $
      ( createRuntime
          { activityUpdate: \_ -> pure unit
          , initialProps: 0
          , initialState: 0
          , spec:
              { eval: case _ of
                  Update _ -> do
                    liftAff $ Aff.finally
                      (void $ AVar.tryPut unit gate.settled)
                      do
                        AVar.put unit gate.started
                        void $ AVar.take gate.release
                    modify_ (_ + 1)
                  _ -> pure unit
              , onError: \_ _ -> pure unit
              , schedule: \_ -> Every
              }
          , stateUpdate: flip Ref.write state
          } :: Effect (Runtime Int Int Unit Unit)
      )

    Aff.finally (liftEffect $ deactivate runtime) do
      liftEffect do
        activate runtime
        updateProps runtime 1
      void $ await "props update evaluation start" gate.started
      liftEffect $ deactivate runtime
      void $ await "props update evaluation cancellation" gate.settled
      value <- liftEffect $ Ref.read state
      value `shouldEqual` 0

  it "cancels structured child tasks with their owning evaluation" $ withHarness \harness -> do
    childStarted <- liftEffect EffectAVar.empty
    childSettled <- liftEffect EffectAVar.empty
    parentRelease <- liftEffect EffectAVar.empty

    liftEffect $ runForTest harness.runtime ActivationError do
      void $ fork do
        liftAff $ Aff.finally
          (void $ AVar.tryPut unit childSettled)
          do
            AVar.put unit childStarted
            void $ AVar.take parentRelease
      liftAff $ void $ AVar.take parentRelease

    void $ await "structured child start" childStarted
    liftEffect $ deactivate harness.runtime
    void $ await "structured child cancellation" childSettled

  it "commit-fences structured children when a Restartable parent is replaced" do
    firstParent <- liftEffect makeGate
    firstChild <- liftEffect makeGate
    replacement <- liftEffect makeGate
    replacementDone <- liftEffect EffectAVar.empty
    state <- liftEffect $ Ref.new 0
    runtime <- liftEffect $ createRuntime
      { activityUpdate: \_ -> pure unit
      , initialProps: unit
      , initialState: 0
      , spec:
          { eval: childEval
          , onError: \_ _ -> pure unit
          , schedule: \_ -> Restartable unit
          }
      , stateUpdate: flip Ref.write state
      }

    Aff.finally (liftEffect $ deactivate runtime) do
      liftEffect do
        activate runtime
        dispatch runtime (ParentWithChild firstParent firstChild)
      void $ await "Restartable parent start" firstParent.started
      void $ await "Restartable child start" firstChild.started

      liftEffect $ dispatch runtime (Replacement replacement replacementDone)
      void $ await "replaced parent cancellation" firstParent.settled
      void $ await "replaced child cancellation" firstChild.settled
      void $ await "replacement parent start" replacement.started
      release replacement
      void $ await "replacement parent state commit" replacementDone

      value <- liftEffect $ Ref.read state
      value `shouldEqual` 10

  it "uses the latest evaluator and handlers after the hook spec changes" $ withHarness \harness -> do
    gate <- liftEffect makeGate
    liftEffect do
      syncSpec harness.runtime
        { activityUpdate: \next -> do
            Ref.write next harness.activity
            void $ EffectAVar.tryPut unit harness.activityChanged
        , spec:
            { eval: case _ of
                Action (Work _ value workGate) -> do
                  liftAff do
                    AVar.put unit workGate.started
                    void $ AVar.take workGate.release
                  modify_ (flip append [ value * 10 ])
                _ -> pure unit
            , onError: \_ _ -> pure unit
            , schedule: policyOf
            }
        , stateUpdate: flip Ref.write harness.state
        }
      dispatch harness.runtime (Work Every 2 gate)

    void $ await "action using replacement evaluator" gate.started
    release gate
    awaitCounts harness { running: 0, queued: 0 }
    state <- liftEffect $ Ref.read harness.state
    state `shouldEqual` [ 20 ]

data ChildAction
  = ParentWithChild Gate Gate
  | Replacement Gate (AVar Unit)

childEval :: Lifecycle Unit ChildAction -> HaloM Unit Int ChildAction Unit Unit
childEval = case _ of
  Activate -> pure unit
  Update _ -> pure unit
  Action (ParentWithChild parent child) -> do
    void $ fork do
      runGate child
      modify_ (_ + 100)
    runGate parent
    modify_ (_ + 1)
  Action (Replacement gate completed) -> do
    runGate gate
    modify_ (_ + 10)
    liftAff $ void $ AVar.tryPut unit completed

runGate :: forall props state action key. Gate -> HaloM props state action key Unit
runGate gate = liftAff $ Aff.finally
  (void $ AVar.tryPut unit gate.settled)
  do
    AVar.put unit gate.started
    void $ AVar.take gate.release

data ReplayAction = Pulse (AVar Unit)

replayEval :: AVar Unit -> Lifecycle Unit ReplayAction -> HaloM Unit Int ReplayAction Unit Unit
replayEval activation = case _ of
  Activate -> do
    modify_ (_ + 1)
    liftAff $ void $ AVar.tryPut unit activation
  Action (Pulse completed) -> do
    modify_ (_ + 10)
    liftAff $ void $ AVar.tryPut unit completed
  Update _ -> pure unit
