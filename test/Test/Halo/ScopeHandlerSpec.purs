module Test.Halo.ScopeHandlerSpec (spec) where

import Prelude

import Control.Monad.State (modify_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff)
import Effect.AVar (AVar)
import Effect.AVar as EffectAVar
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import React.Halo.Handlers (defaultHandlers)
import React.Halo.Internal.Runtime (HaloM, Runtime, activate, createRuntime, deactivate, dispatch, fork, props, startTask, syncSpec, updateProps)
import React.Halo.Internal.Types (TaskPolicy(..))
import Test.Halo.Helpers (Action(..), Gate, Key(..), await, awaitCounts, makeGate, release, shouldNotHaveStarted, withHarness)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "scope and handlers" do
  it "cancels explicit tasks on deactivation and accepts work after reactivation" $ withHarness \harness -> do
    running <- liftEffect makeGate
    queued <- liftEffect makeGate
    ignored <- liftEffect makeGate

    liftEffect do
      dispatch harness.runtime (StartTask (Enqueue Save) 1 running)
      dispatch harness.runtime (StartTask (Enqueue Save) 2 queued)
    void $ await "running task before deactivation" running.started
    awaitCounts harness { running: 1, queued: 1 }

    liftEffect $ deactivate harness.runtime
    void $ await "running task cancellation on deactivation" running.settled
    awaitCounts harness { running: 0, queued: 0 }
    shouldNotHaveStarted queued

    liftEffect $ dispatch harness.runtime (StartTask Every 3 ignored)
    shouldNotHaveStarted ignored
    state <- liftEffect $ Ref.read harness.state
    state `shouldEqual` []

    reactivated <- liftEffect makeGate
    liftEffect do
      activate harness.runtime
      dispatch harness.runtime (StartTask Every 4 reactivated)
    void $ await "task start after reactivation" reactivated.started
    release reactivated
    void $ await "task completion after reactivation" reactivated.settled
    awaitCounts harness { running: 0, queued: 0 }
    reactivatedState <- liftEffect $ Ref.read harness.state
    reactivatedState `shouldEqual` [ 4 ]

  it "models StrictMode setup-cleanup-setup with repeatable onActivate" do
    activation <- liftEffect EffectAVar.empty
    state <- liftEffect $ Ref.new 0
    runtime <- liftEffect $
      ( createRuntime
          { activityUpdate: \_ -> pure unit
          , initialProps: unit
          , initialState: 0
          , spec:
              { handlers: defaultHandlers
                  { onActivate = do
                      modify_ (_ + 1)
                      liftAff $ void $ AVar.tryPut unit activation
                  , onAction = \(Pulse completed) -> do
                      modify_ (_ + 10)
                      liftAff $ void $ AVar.tryPut unit completed
                  }
              , onError: \_ _ -> pure unit
              }
          , stateUpdate: flip Ref.write state
          } :: Effect (Runtime Unit Int ReplayAction Unit)
      )

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

  it "passes previous props and exposes current props to onPropsChange" do
    changed <- liftEffect EffectAVar.empty
    runtime <- liftEffect $
      ( createRuntime
          { activityUpdate: \_ -> pure unit
          , initialProps: 0
          , initialState: unit
          , spec:
              { handlers: defaultHandlers
                  { onPropsChange = \previous -> do
                      current <- props
                      liftAff $ void $ AVar.tryPut (Tuple previous current) changed
                  }
              , onError: \_ _ -> pure unit
              }
          , stateUpdate: \_ -> pure unit
          } :: Effect (Runtime Int Unit Unit Unit)
      )

    Aff.finally (liftEffect $ deactivate runtime) do
      liftEffect do
        activate runtime
        updateProps runtime 1
      values <- await "props change handler" changed
      values `shouldEqual` Tuple 0 1

  it "owns and cancels a running props-change handler" do
    gate <- liftEffect makeGate
    state <- liftEffect $ Ref.new 0
    runtime <- liftEffect $
      ( createRuntime
          { activityUpdate: \_ -> pure unit
          , initialProps: 0
          , initialState: 0
          , spec:
              { handlers: defaultHandlers
                  { onPropsChange = \_ -> do
                      runIntGate gate
                      modify_ (_ + 1)
                  }
              , onError: \_ _ -> pure unit
              }
          , stateUpdate: flip Ref.write state
          } :: Effect (Runtime Int Int Unit Unit)
      )

    Aff.finally (liftEffect $ deactivate runtime) do
      liftEffect do
        activate runtime
        updateProps runtime 1
      void $ await "props-change handler start" gate.started
      liftEffect $ deactivate runtime
      void $ await "props-change handler cancellation" gate.settled
      value <- liftEffect $ Ref.read state
      value `shouldEqual` 0

  it "cancels a structured fork when its action handler finishes" do
    child <- liftEffect makeGate
    handlerDone <- liftEffect EffectAVar.empty
    state <- liftEffect $ Ref.new 0
    runtime <- liftEffect $
      ( createRuntime
          { activityUpdate: \_ -> pure unit
          , initialProps: unit
          , initialState: 0
          , spec:
              { handlers: defaultHandlers
                  { onAction = \(ForkAndReturn gate completed) -> do
                      void $ fork do
                        runIntGate gate
                        modify_ (_ + 100)
                      liftAff $ void $ AVar.take gate.started
                      liftAff $ void $ AVar.tryPut unit completed
                  }
              , onError: \_ _ -> pure unit
              }
          , stateUpdate: flip Ref.write state
          } :: Effect (Runtime Unit Int ForkAction Unit)
      )

    Aff.finally (liftEffect $ deactivate runtime) do
      liftEffect do
        activate runtime
        dispatch runtime (ForkAndReturn child handlerDone)
      void $ await "action handler return" handlerDone
      void $ await "structured child cancellation" child.settled
      value <- liftEffect $ Ref.read state
      value `shouldEqual` 0

  it "commit-fences a task and its structured child when replaced" do
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
          { handlers: defaultHandlers
              { onAction = case _ of
                  ParentTask parent child -> startTask (Restartable unit) do
                    void $ fork do
                      runIntGate child
                      modify_ (_ + 100)
                    liftAff $ void $ AVar.take child.started
                    runIntGate parent
                    modify_ (_ + 1)
                  ReplacementTask gate completed -> startTask (Restartable unit) do
                    runIntGate gate
                    modify_ (_ + 10)
                    liftAff $ void $ AVar.tryPut unit completed
              }
          , onError: \_ _ -> pure unit
          }
      , stateUpdate: flip Ref.write state
      }

    Aff.finally (liftEffect $ deactivate runtime) do
      liftEffect do
        activate runtime
        dispatch runtime (ParentTask firstParent firstChild)
      void $ await "parent task start" firstParent.started

      liftEffect $ dispatch runtime (ReplacementTask replacement replacementDone)
      void $ await "replaced parent task cancellation" firstParent.settled
      void $ await "replaced structured child cancellation" firstChild.settled
      void $ await "replacement task start" replacement.started
      release replacement
      void $ await "replacement task completion" replacementDone

      value <- liftEffect $ Ref.read state
      value `shouldEqual` 10

  it "uses the latest handlers after the hook spec changes" $ withHarness \harness -> do
    gate <- liftEffect makeGate
    liftEffect do
      syncSpec harness.runtime
        { activityUpdate: \next -> do
            Ref.write next harness.activity
            void $ EffectAVar.tryPut unit harness.activityChanged
        , spec:
            { handlers: defaultHandlers
                { onAction = case _ of
                    Direct value workGate -> do
                      liftAff $ void $ AVar.tryPut unit workGate.launched
                      liftAff do
                        AVar.put unit workGate.started
                        void $ AVar.take workGate.release
                      modify_ (flip append [ value * 10 ])
                      liftAff $ void $ AVar.tryPut unit workGate.settled
                    _ -> pure unit
                }
            , onError: \_ _ -> pure unit
            }
        , stateUpdate: flip Ref.write harness.state
        }
      dispatch harness.runtime (Direct 2 gate)

    void $ await "action using replacement handler" gate.started
    release gate
    void $ await "replacement handler completion" gate.settled
    awaitCounts harness { running: 0, queued: 0 }
    state <- liftEffect $ Ref.read harness.state
    state `shouldEqual` [ 20 ]

data ReplayAction = Pulse (AVar Unit)

data ForkAction = ForkAndReturn Gate (AVar Unit)

data ParentAction
  = ParentTask Gate Gate
  | ReplacementTask Gate (AVar Unit)

runIntGate :: forall props action key. Gate -> HaloM props Int action key Unit
runIntGate gate = do
  liftAff $ Aff.finally
    (void $ AVar.tryPut unit gate.settled)
    do
      AVar.put unit gate.started
      void $ AVar.take gate.release
