module Test.Halo.ScopeHandlerSpec (spec) where

import Prelude

import Control.Monad.State (modify_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff)
import Effect.AVar (AVar)
import Effect.AVar as EffectAVar
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import React.Halo.Handlers (defaultHandlers)
import React.Halo.Internal.Runtime (Runtime, activate, createRuntime, deactivate, dispatch, fork, getProps, kill, syncSpec, updateProps)
import React.Halo.Internal.Types (ForkId)
import Test.Halo.Helpers (Gate, await, makeGate, release, shouldNotHaveStarted, waitForGate)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

identityAff :: Aff ~> Aff
identityAff = identity

spec :: Spec Unit
spec = describe "scope, handlers, and component-owned forks" do
  it "cancels roots on deactivation and accepts work after reactivation" do
    forkGate <- liftEffect makeGate
    forkId <- liftEffect EffectAVar.empty
    handlerDone <- liftEffect EffectAVar.empty
    pulseDone <- liftEffect EffectAVar.empty
    state <- liftEffect $ Ref.new 0
    runtime <- liftEffect $
      ( createRuntime identityAff
          { initialProps: unit
          , initialState: 0
          , spec:
              { handlers: defaultHandlers
                  { onAction = case _ of
                      StartScopedFork gate fid completed -> do
                        child <- fork do
                          liftAff $ waitForGate gate
                          modify_ (_ + 1)
                        liftAff $ void $ AVar.tryPut child fid
                        liftAff $ void $ AVar.tryPut unit completed
                      Pulse completed -> do
                        modify_ (_ + 10)
                        liftAff $ void $ AVar.tryPut unit completed
                  }
              , onError: \_ _ -> pure unit
              }
          , stateUpdate: \next _ -> Ref.write next state
          } :: Effect (Runtime Unit Int ScopeAction Aff)
      )

    liftEffect do
      activate runtime
      dispatch runtime (StartScopedFork forkGate forkId handlerDone)
    void $ await "fork id" forkId
    void $ await "launching handler completion" handlerDone
    void $ await "fork before deactivation" forkGate.started

    liftEffect $ deactivate runtime
    void $ await "fork cancellation on deactivation" forkGate.settled
    valueAfterDeactivate <- liftEffect $ Ref.read state
    valueAfterDeactivate `shouldEqual` 0

    ignored <- liftEffect makeGate
    ignoredId <- liftEffect EffectAVar.empty
    ignoredDone <- liftEffect EffectAVar.empty
    liftEffect $ dispatch runtime (StartScopedFork ignored ignoredId ignoredDone)
    shouldNotHaveStarted ignored

    liftEffect do
      activate runtime
      dispatch runtime (Pulse pulseDone)
    void $ await "action after reactivation" pulseDone
    valueAfterReactivate <- liftEffect $ Ref.read state
    valueAfterReactivate `shouldEqual` 10
    liftEffect $ deactivate runtime

  it "models StrictMode setup-cleanup-setup with repeatable onActivate" do
    activation <- liftEffect EffectAVar.empty
    pulse <- liftEffect EffectAVar.empty
    state <- liftEffect $ Ref.new 0
    runtime <- liftEffect $
      ( createRuntime identityAff
          { initialProps: unit
          , initialState: 0
          , spec:
              { handlers: defaultHandlers
                  { onActivate = do
                      modify_ (_ + 1)
                      liftAff $ void $ AVar.tryPut unit activation
                  , onAction = \(ReplayPulse completed) -> do
                      modify_ (_ + 10)
                      liftAff $ void $ AVar.tryPut unit completed
                  }
              , onError: \_ _ -> pure unit
              }
          , stateUpdate: \next _ -> Ref.write next state
          } :: Effect (Runtime Unit Int ReplayAction Aff)
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
      liftEffect $ dispatch runtime (ReplayPulse pulse)
      void $ await "action after StrictMode replay" pulse
      second <- liftEffect $ Ref.read state
      second `shouldEqual` 12

  it "passes previous props and exposes current props" do
    changed <- liftEffect EffectAVar.empty
    runtime <- liftEffect $
      ( createRuntime identityAff
          { initialProps: 0
          , initialState: unit
          , spec:
              { handlers: defaultHandlers
                  { onPropsChange = \previous -> do
                      current <- getProps
                      liftAff $ void $ AVar.tryPut (Tuple previous current) changed
                  }
              , onError: \_ _ -> pure unit
              }
          , stateUpdate: \_ _ -> pure unit
          } :: Effect (Runtime Int Unit Unit Aff)
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
      ( createRuntime identityAff
          { initialProps: 0
          , initialState: 0
          , spec:
              { handlers: defaultHandlers
                  { onPropsChange = \_ -> do
                      liftAff $ waitForGate gate
                      modify_ (_ + 1)
                  }
              , onError: \_ _ -> pure unit
              }
          , stateUpdate: \next _ -> Ref.write next state
          } :: Effect (Runtime Int Int Unit Aff)
      )

    liftEffect do
      activate runtime
      updateProps runtime 1
    void $ await "props-change handler start" gate.started
    liftEffect $ deactivate runtime
    void $ await "props-change handler cancellation" gate.settled
    value <- liftEffect $ Ref.read state
    value `shouldEqual` 0

  it "lets a component-owned fork outlive its launching handler" do
    child <- liftEffect makeGate
    forkId <- liftEffect EffectAVar.empty
    handlerDone <- liftEffect EffectAVar.empty
    state <- liftEffect $ Ref.new 0
    runtime <- liftEffect $
      ( createRuntime identityAff
          { initialProps: unit
          , initialState: 0
          , spec:
              { handlers: defaultHandlers
                  { onAction = \(LaunchChild gate fid completed) -> do
                      childId <- fork do
                        liftAff $ waitForGate gate
                        modify_ (_ + 1)
                      liftAff $ void $ AVar.tryPut childId fid
                      liftAff $ void $ AVar.tryPut unit completed
                  }
              , onError: \_ _ -> pure unit
              }
          , stateUpdate: \next _ -> Ref.write next state
          } :: Effect (Runtime Unit Int ForkAction Aff)
      )

    Aff.finally (liftEffect $ deactivate runtime) do
      liftEffect do
        activate runtime
        dispatch runtime (LaunchChild child forkId handlerDone)
      void $ await "child start" child.started
      void $ await "launching handler return" handlerDone
      settledBeforeRelease <- liftEffect $ EffectAVar.tryTake child.settled
      settledBeforeRelease `shouldEqual` Nothing

      release child
      void $ await "child completion" child.settled
      value <- liftEffect $ Ref.read state
      value `shouldEqual` 1

  it "kill fences commits and capabilities, and waits for finalizers" do
    child <- liftEffect makeGate
    ignored <- liftEffect makeGate
    forkId <- liftEffect EffectAVar.empty
    launchDone <- liftEffect EffectAVar.empty
    killDone <- liftEffect EffectAVar.empty
    state <- liftEffect $ Ref.new 0
    runtime <- liftEffect $
      ( createRuntime identityAff
          { initialProps: unit
          , initialState: 0
          , spec:
              { handlers: defaultHandlers
                  { onAction = case _ of
                      LaunchCancellable gate ignoredGate fid completed -> do
                        childId <- fork do
                          liftAff $ Aff.catchError (waitForGate gate) (\_ -> pure unit)
                          void $ fork $ liftAff $ waitForGate ignoredGate
                          modify_ (_ + 1)
                        liftAff $ void $ AVar.tryPut childId fid
                        liftAff $ void $ AVar.tryPut unit completed
                      KillChild fid completed -> do
                        kill fid
                        liftAff $ void $ AVar.tryPut unit completed
                  }
              , onError: \_ _ -> pure unit
              }
          , stateUpdate: \next _ -> Ref.write next state
          } :: Effect (Runtime Unit Int CancelAction Aff)
      )

    Aff.finally (liftEffect $ deactivate runtime) do
      liftEffect do
        activate runtime
        dispatch runtime (LaunchCancellable child ignored forkId launchDone)
      fid <- await "cancellable fork id" forkId
      void $ await "cancellable fork launch" launchDone
      void $ await "cancellable fork start" child.started

      liftEffect $ dispatch runtime (KillChild fid killDone)
      void $ await "kill completion" killDone
      finalizerRan <- liftEffect $ EffectAVar.tryTake child.settled
      finalizerRan `shouldEqual` Just unit
      shouldNotHaveStarted ignored
      value <- liftEffect $ Ref.read state
      value `shouldEqual` 0

  it "uses the latest handlers for new actions" do
    oldCompleted <- liftEffect EffectAVar.empty
    newCompleted <- liftEffect EffectAVar.empty
    state <- liftEffect $ Ref.new 0
    let
      oldHandlers = defaultHandlers
        { onAction = \Refresh -> do
            modify_ (_ + 1)
            liftAff $ void $ AVar.tryPut unit oldCompleted
        }
      newHandlers = defaultHandlers
        { onAction = \Refresh -> do
            modify_ (_ + 10)
            liftAff $ void $ AVar.tryPut unit newCompleted
        }
    runtime <- liftEffect $
      ( createRuntime identityAff
          { initialProps: unit
          , initialState: 0
          , spec: { handlers: oldHandlers, onError: \_ _ -> pure unit }
          , stateUpdate: \next _ -> Ref.write next state
          } :: Effect (Runtime Unit Int RefreshAction Aff)
      )

    Aff.finally (liftEffect $ deactivate runtime) do
      liftEffect do
        activate runtime
        syncSpec runtime identityAff
          { spec: { handlers: newHandlers, onError: \_ _ -> pure unit }
          , stateUpdate: \next _ -> Ref.write next state
          }
        dispatch runtime Refresh
      void $ await "new action handler" newCompleted
      oldRan <- liftEffect $ EffectAVar.tryTake oldCompleted
      oldRan `shouldEqual` Nothing
      value <- liftEffect $ Ref.read state
      value `shouldEqual` 10

data ScopeAction
  = StartScopedFork Gate (AVar ForkId) (AVar Unit)
  | Pulse (AVar Unit)

data ReplayAction = ReplayPulse (AVar Unit)

data ForkAction = LaunchChild Gate (AVar ForkId) (AVar Unit)

data CancelAction
  = LaunchCancellable Gate Gate (AVar ForkId) (AVar Unit)
  | KillChild ForkId (AVar Unit)

data RefreshAction = Refresh
