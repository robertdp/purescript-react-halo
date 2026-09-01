module Test.Halo.SubscriptionErrorSpec (spec) where

import Prelude

import Control.Monad.State (get, modify_, put)
import Data.Foldable (foldl, traverse_)
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
import React.Halo.Internal.Runtime (Runtime, activate, createRuntime, deactivate, dispatch, fork, registerCleanup, releaseCleanup, subscribe, syncSpec, unsubscribe)
import React.Halo.Internal.Types (CleanupId(..), ErrorContext(..), ForkId, SubscriptionId)
import React.Halo.Subscription (Emitter, makeEmitter)
import Test.Halo.Helpers (Gate, await, makeGate, release, waitForGate)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

identityAff :: Aff ~> Aff
identityAff = identity

spec :: Spec Unit
spec = describe "subscriptions, cleanup, and errors" do
  it "releases generic cleanup once and ignores unknown IDs" do
    cleanupCount <- liftEffect $ Ref.new 0
    registered <- liftEffect EffectAVar.empty
    released <- liftEffect EffectAVar.empty
    unknownReleased <- liftEffect EffectAVar.empty
    runtime <- liftEffect $ createRuntime identityAff
      { initialProps: unit
      , initialState: Nothing
      , spec: { handlers: subscriptionHandlers, onError: \_ _ -> pure unit }
      , stateUpdate: \_ _ -> pure unit
      }

    Aff.finally (liftEffect $ deactivate runtime) do
      liftEffect do
        activate runtime
        dispatch runtime (RegisterCleanup (Ref.modify_ (_ + 1) cleanupCount) registered)
      cid <- await "generic cleanup registration" registered
      liftEffect $ dispatch runtime (ReleaseCleanup cid released)
      void $ await "manual generic cleanup release" released
      liftEffect $ dispatch runtime (ReleaseCleanup (CleanupId 999_999) unknownReleased)
      void $ await "unknown generic cleanup release" unknownReleased
      liftEffect $ dispatch runtime (ReleaseCleanup cid released)
      void $ await "already released generic cleanup" released
      liftEffect $ deactivate runtime
      actual <- liftEffect $ Ref.read cleanupCount
      actual `shouldEqual` 1

  it "does not retry a throwing manual cleanup release" do
    cleanupRuns <- liftEffect $ Ref.new 0
    registered <- liftEffect EffectAVar.empty
    releaseFailed <- liftEffect EffectAVar.empty
    errors <- liftEffect $ Ref.new []
    runtime <- liftEffect $ createRuntime identityAff
      { initialProps: unit
      , initialState: Nothing
      , spec:
          { handlers: subscriptionHandlers
          , onError: \context error -> do
              let
                label = case context of
                  ActionError (ReleaseCleanup _ _) -> "release"
                  _ -> "wrong context"
              Ref.modify_ (_ <> [ label <> ": " <> Exception.message error ]) errors
              void $ EffectAVar.tryPut unit releaseFailed
          }
      , stateUpdate: \_ _ -> pure unit
      }

    liftEffect do
      activate runtime
      dispatch runtime
        ( RegisterCleanup
            (Ref.modify_ (_ + 1) cleanupRuns *> Exception.throw "manual cleanup failed")
            registered
        )
    cid <- await "throwing cleanup registration" registered
    completed <- liftEffect EffectAVar.empty
    liftEffect $ dispatch runtime (ReleaseCleanup cid completed)
    void $ await "throwing manual cleanup error" releaseFailed
    liftEffect $ deactivate runtime
    runs <- liftEffect $ Ref.read cleanupRuns
    actual <- liftEffect $ Ref.read errors
    runs `shouldEqual` 1
    actual `shouldEqual` [ "release: manual cleanup failed" ]

  it "rejects cleanup registration retained by a stale activation" do
    cleanupCount <- liftEffect $ Ref.new 0
    gate <- liftEffect makeGate
    runtime <- liftEffect $
      ( createRuntime identityAff
          { initialProps: unit
          , initialState: unit
          , spec:
              { handlers: staleCleanupHandlers
              , onError: \_ _ -> pure unit
              }
          , stateUpdate: \_ _ -> pure unit
          } :: Effect (Runtime Unit Unit StaleCleanupAction Aff)
      )

    liftEffect do
      activate runtime
      dispatch runtime (RegisterAfterCancellation gate (Ref.modify_ (_ + 1) cleanupCount))
    void $ await "stale cleanup handler" gate.started
    liftEffect $ deactivate runtime
    void $ await "stale cleanup cancellation" gate.settled
    liftEffect do
      activate runtime
      deactivate runtime
    actual <- liftEffect $ Ref.read cleanupCount
    actual `shouldEqual` 0

  it "keeps generic cleanup IDs scoped to one StrictMode activation" do
    cleanupCount <- liftEffect $ Ref.new 0
    firstRegistered <- liftEffect EffectAVar.empty
    staleReleased <- liftEffect EffectAVar.empty
    secondRegistered <- liftEffect EffectAVar.empty
    runtime <- liftEffect $ createRuntime identityAff
      { initialProps: unit
      , initialState: Nothing
      , spec: { handlers: subscriptionHandlers, onError: \_ _ -> pure unit }
      , stateUpdate: \_ _ -> pure unit
      }

    liftEffect do
      activate runtime
      dispatch runtime (RegisterCleanup (Ref.modify_ (_ + 1) cleanupCount) firstRegistered)
    staleId <- await "first activation cleanup" firstRegistered
    liftEffect do
      deactivate runtime
      activate runtime
      dispatch runtime (ReleaseCleanup staleId staleReleased)
    void $ await "stale cleanup release" staleReleased
    liftEffect $ dispatch runtime
      (RegisterCleanup (Ref.modify_ (_ + 1) cleanupCount) secondRegistered)
    void $ await "second activation cleanup" secondRegistered
    liftEffect $ deactivate runtime
    actual <- liftEffect $ Ref.read cleanupCount
    actual `shouldEqual` 2

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
      , stateUpdate: \next _ -> Ref.write next state
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
      , stateUpdate: \next _ -> Ref.write next state
      }

    Aff.finally (liftEffect $ deactivate runtime) do
      liftEffect do
        activate runtime
        dispatch runtime (Start emitter started)
      void $ await "tracked subscription setup" started
      liftEffect $ deactivate runtime
      cleaned <- liftEffect $ Ref.read cleanupCount
      cleaned `shouldEqual` 1

  it "isolates cleanup failures across generic and subscription resources" do
    cleaned <- liftEffect $ Ref.new 0
    oldErrors <- liftEffect $ Ref.new []
    cleanupErrors <- liftEffect $ Ref.new []
    badStarted <- liftEffect EffectAVar.empty
    goodStarted <- liftEffect EffectAVar.empty
    badCleanupRegistered <- liftEffect EffectAVar.empty
    goodCleanupRegistered <- liftEffect EffectAVar.empty
    state <- liftEffect $ Ref.new Nothing
    gate <- liftEffect makeGate
    let
      badEmitter = makeEmitter \_ -> pure $ Exception.throw "subscription cleanup failed"
      goodEmitter = makeEmitter \_ -> pure $ Ref.modify_ (_ + 1) cleaned
      onCleanupError target context error = case context of
        DeactivationError -> Ref.modify_ (_ <> [ Exception.message error ]) target
        _ -> Ref.modify_ (_ <> [ "wrong error context" ]) target

    runtime <- liftEffect $ createRuntime identityAff
      { initialProps: unit
      , initialState: Nothing
      , spec:
          { handlers: subscriptionHandlers
          , onError: onCleanupError oldErrors
          }
      , stateUpdate: \next _ -> Ref.write next state
      }

    liftEffect do
      activate runtime
      dispatch runtime (Start badEmitter badStarted)
    void $ await "failing subscription setup" badStarted
    liftEffect $ dispatch runtime (Start goodEmitter goodStarted)
    void $ await "successful subscription setup" goodStarted
    liftEffect $ dispatch runtime
      (RegisterCleanup (Exception.throw "generic cleanup failed") badCleanupRegistered)
    void $ await "failing generic cleanup setup" badCleanupRegistered
    liftEffect $ dispatch runtime
      (RegisterCleanup (Ref.modify_ (_ + 1) cleaned) goodCleanupRegistered)
    void $ await "successful generic cleanup setup" goodCleanupRegistered
    liftEffect $ dispatch runtime (Block gate)
    void $ await "running action handler" gate.started

    liftEffect do
      syncSpec runtime identityAff
        { spec:
            { handlers: subscriptionHandlers
            , onError: onCleanupError cleanupErrors
            }
        , stateUpdate: \next _ -> Ref.write next state
        }
      deactivate runtime
    void $ await "running action cancellation" gate.settled

    cleanupCount <- liftEffect $ Ref.read cleaned
    cleanupCount `shouldEqual` 2
    previous <- liftEffect $ Ref.read oldErrors
    previous `shouldEqual` []
    errors <- liftEffect $ Ref.read cleanupErrors
    foldl (\count _ -> count + 1) 0 errors `shouldEqual` 2
    foldl (\found message -> found || message == "generic cleanup failed") false errors `shouldEqual` true
    foldl (\found message -> found || message == "subscription cleanup failed") false errors `shouldEqual` true

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
          , stateUpdate: \next _ -> Ref.write next state
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
        , stateUpdate: \_ _ -> pure unit
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
          , stateUpdate: \_ _ -> pure unit
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
  | RegisterCleanup (Effect Unit) (AVar CleanupId)
  | ReleaseCleanup CleanupId (AVar Unit)

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
      RegisterCleanup cleanup registered -> do
        cid <- registerCleanup cleanup
        liftAff $ void $ AVar.tryPut cid registered
      ReleaseCleanup cid completed -> do
        releaseCleanup cid
        liftAff $ void $ AVar.tryPut unit completed
  }

data StaleCleanupAction = RegisterAfterCancellation Gate (Effect Unit)

staleCleanupHandlers :: Handlers Unit Unit StaleCleanupAction Aff
staleCleanupHandlers = defaultHandlers
  { onAction = \(RegisterAfterCancellation gate cleanup) -> do
      liftAff $ Aff.catchError (waitForGate gate) (\_ -> pure unit)
      void $ registerCleanup cleanup
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
  , stateUpdate: \_ _ -> pure unit
  }
