module Test.Halo.TaskSpec (spec) where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (modify_)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Lens (Lens', preview, review)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff)
import Effect.AVar (AVar)
import Effect.AVar as EffectAVar
import Effect.Class (liftEffect)
import Effect.Exception as Exception
import Effect.Ref as Ref
import React.Halo.Handlers (Handlers, defaultHandlers)
import React.Halo.Internal.Runtime (HaloM, Runtime, activate, createRuntime, deactivate, dispatch, syncSpec)
import React.Halo.Internal.Task as TaskInternal
import React.Halo.Internal.Types (ErrorContext(..))
import React.Halo.Task as Task
import Test.Halo.Helpers (Gate, await, makeGate, release, shouldNotHaveStarted, waitForGate)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

type ComponentState =
  { elsewhere :: Int
  , task :: Task.State String Int
  }

taskLens :: Lens' ComponentState (Task.State String Int)
taskLens = prop (Proxy :: Proxy "task")

data Body
  = WaitBody Gate (Either String Int)
  | UpdateBody Gate Int (Either String Int)
  | UnexpectedBody Gate
  | CancellableBody Gate Gate (Ref.Ref Boolean)

type Timer =
  { duration :: AVar Milliseconds
  , gate :: Gate
  }

data Action
  = RunOnce Body (AVar Unit)
  | RunStartIfInactive Body (AVar Unit)
  | RunStartTwice Body Body (AVar Unit)
  | RunSupersede Body (AVar Unit)
  | RunDebounce Timer Milliseconds Body (AVar Unit)
  | Reset (AVar Unit)

type UI a = HaloM Unit ComponentState Action Aff a

handlers :: Handlers Unit ComponentState Action Aff
handlers = defaultHandlers
  { onAction = case _ of
      RunOnce body launched -> do
        Task.once taskLens (runBody body)
        signal launched
      RunStartIfInactive body launched -> do
        Task.startIfInactive taskLens (runBody body)
        signal launched
      RunStartTwice first second launched -> do
        Task.startIfInactive taskLens (runBody first)
        Task.startIfInactive taskLens (runBody second)
        signal launched
      RunSupersede body launched -> do
        Task.supersede taskLens (runBody body)
        signal launched
      RunDebounce timer duration body launched -> do
        TaskInternal.debounceWith (runTimer timer) taskLens duration (runBody body)
        signal launched
      Reset completed -> do
        Task.reset taskLens
        signal completed
  }

runBody :: Body -> UI (Either String Int)
runBody = case _ of
  WaitBody gate outcome -> do
    liftAff $ waitForGate gate
    pure outcome
  UpdateBody gate amount outcome -> do
    liftAff $ waitForGate gate
    modify_ \state -> state { elsewhere = state.elsewhere + amount }
    pure outcome
  UnexpectedBody gate -> do
    liftAff $ waitForGate gate
    liftAff $ Aff.throwError (Aff.error "task boom")
  CancellableBody work finalizer externalWitness -> do
    liftAff $ Aff.catchError
      (Aff.finally (waitForGate finalizer) (waitForGate work))
      (\_ -> pure unit)
    modify_ \state -> state { elsewhere = state.elsewhere + 100 }
    liftEffect $ Ref.write true externalWitness
    pure (Right 999)

runTimer :: Timer -> Milliseconds -> Aff Unit
runTimer timer duration = do
  AVar.put duration timer.duration
  waitForGate timer.gate

signal :: AVar Unit -> UI Unit
signal completed = liftAff $ void $ AVar.tryPut unit completed

type Harness =
  { runtime :: Runtime Unit ComponentState Action Aff
  , setterCalls :: Ref.Ref Int
  , state :: Ref.Ref ComponentState
  , updates :: AVar (Task.Status String Int)
  }

makeHarness
  :: (ErrorContext Unit Action -> Exception.Error -> Effect Unit)
  -> Aff Harness
makeHarness onError = liftEffect do
  state <- Ref.new initialState
  updates <- EffectAVar.empty
  setterCalls <- Ref.new 0
  runtime <- createRuntime identity
    { initialProps: unit
    , initialState
    , spec: { handlers, onError }
    , stateUpdate: updateState state updates setterCalls
    }
  activate runtime
  pure { runtime, setterCalls, state, updates }

initialState :: ComponentState
initialState = { elsewhere: 0, task: Task.idle }

updateState
  :: Ref.Ref ComponentState
  -> AVar (Task.Status String Int)
  -> Ref.Ref Int
  -> ComponentState
  -> Effect Unit
updateState state updates setterCalls next = do
  previous <- Ref.read state
  Ref.write next state
  Ref.modify_ (_ + 1) setterCalls
  let
    previousStatus = Task.toStatus previous.task
    nextStatus = Task.toStatus next.task
  when (previousStatus /= nextStatus) do
    void $ EffectAVar.tryPut nextStatus updates

awaitStatus
  :: String
  -> Task.Status String Int
  -> AVar (Task.Status String Int)
  -> Aff Unit
awaitStatus label expected updates = do
  actual <- await label updates
  if actual == expected then pure unit
  else awaitStatus label expected updates

makeTimer :: Effect Timer
makeTimer = do
  gate <- makeGate
  duration <- EffectAVar.empty
  pure { duration, gate }

statusOf :: Harness -> Effect (Task.Status String Int)
statusOf harness = Task.toStatus <<< _.task <$> Ref.read harness.state

spec :: Spec Unit
spec = describe "state-focused tasks" do
  it "projects status through helpers and lawful prisms" do
    Task.toStatus (Task.idle :: Task.State String Int) `shouldEqual` Task.Idle
    Task.toMaybe (Task.idle :: Task.State String Int) `shouldEqual` Nothing
    Task.isActive (Task.idle :: Task.State String Int) `shouldEqual` false
    preview (Task.asStatus <<< Task._Idle) (Task.idle :: Task.State String Int) `shouldEqual` Just unit
    preview Task._Idle (Task.Idle :: Task.Status String Int) `shouldEqual` Just unit
    preview Task._Active (Task.Active :: Task.Status String Int) `shouldEqual` Just unit
    preview Task._Failed (Task.Failed "no" :: Task.Status String Int) `shouldEqual` Just "no"
    preview Task._Succeeded (Task.Succeeded 4 :: Task.Status String Int) `shouldEqual` Just 4
    review Task._Failed "bad" `shouldEqual` (Task.Failed "bad" :: Task.Status String Int)
    review Task._Succeeded 5 `shouldEqual` (Task.Succeeded 5 :: Task.Status String Int)

  it "keeps once terminal until reset and stores typed outcomes" do
    harness <- makeHarness \_ _ -> pure unit
    Aff.finally (liftEffect $ deactivate harness.runtime) do
      first <- liftEffect makeGate
      ignored <- liftEffect makeGate
      failed <- liftEffect makeGate
      firstLaunched <- liftEffect EffectAVar.empty
      ignoredLaunched <- liftEffect EffectAVar.empty
      failedLaunched <- liftEffect EffectAVar.empty
      resetDone <- liftEffect EffectAVar.empty

      liftEffect $ dispatch harness.runtime (RunOnce (WaitBody first (Right 1)) firstLaunched)
      void $ await "once launch" firstLaunched
      void $ await "once body" first.started
      awaitStatus "once active" Task.Active harness.updates

      liftEffect $ dispatch harness.runtime (RunOnce (WaitBody ignored (Right 2)) ignoredLaunched)
      void $ await "ignored active once" ignoredLaunched
      shouldNotHaveStarted ignored

      release first
      void $ await "once body settlement" first.settled
      awaitStatus "once success" (Task.Succeeded 1) harness.updates
      liftEffect $ dispatch harness.runtime (RunOnce (WaitBody ignored (Right 2)) ignoredLaunched)
      shouldNotHaveStarted ignored

      liftEffect $ dispatch harness.runtime (Reset resetDone)
      void $ await "terminal reset" resetDone
      awaitStatus "reset idle" Task.Idle harness.updates

      liftEffect $ dispatch harness.runtime (RunOnce (WaitBody failed (Left "expected")) failedLaunched)
      void $ await "once after reset" failedLaunched
      void $ await "failed body" failed.started
      awaitStatus "once active after reset" Task.Active harness.updates
      release failed
      awaitStatus "typed failure" (Task.Failed "expected") harness.updates

      another <- liftEffect makeGate
      liftEffect $ dispatch harness.runtime (RunOnce (WaitBody another (Right 3)) ignoredLaunched)
      shouldNotHaveStarted another

  it "starts from terminal states only when inactive and claims duplicates atomically" do
    harness <- makeHarness \_ _ -> pure unit
    Aff.finally (liftEffect $ deactivate harness.runtime) do
      first <- liftEffect makeGate
      ignored <- liftEffect makeGate
      second <- liftEffect makeGate
      third <- liftEffect makeGate
      launched <- liftEffect EffectAVar.empty

      liftEffect $ dispatch harness.runtime
        (RunStartTwice (WaitBody first (Left "first")) (WaitBody ignored (Right 99)) launched)
      void $ await "same-turn start calls" launched
      void $ await "first inactive body" first.started
      shouldNotHaveStarted ignored
      awaitStatus "start active" Task.Active harness.updates
      release first
      awaitStatus "start typed failure" (Task.Failed "first") harness.updates

      liftEffect $ dispatch harness.runtime (RunStartIfInactive (WaitBody second (Right 2)) launched)
      void $ await "terminal rerun body" second.started
      awaitStatus "terminal replaced" Task.Active harness.updates
      release second
      awaitStatus "terminal success" (Task.Succeeded 2) harness.updates

      liftEffect $ dispatch harness.runtime (RunStartIfInactive (WaitBody third (Right 3)) launched)
      void $ await "success rerun body" third.started
      awaitStatus "success replaced" Task.Active harness.updates
      release third
      awaitStatus "second success" (Task.Succeeded 3) harness.updates

  it "allows bodies to update unrelated component state" do
    harness <- makeHarness \_ _ -> pure unit
    Aff.finally (liftEffect $ deactivate harness.runtime) do
      gate <- liftEffect makeGate
      launched <- liftEffect EffectAVar.empty
      liftEffect $ dispatch harness.runtime (RunStartIfInactive (UpdateBody gate 7 (Right 8)) launched)
      void $ await "updating body" gate.started
      awaitStatus "updating task active" Task.Active harness.updates
      release gate
      awaitStatus "updating task success" (Task.Succeeded 8) harness.updates
      state <- liftEffect $ Ref.read harness.state
      state.elsewhere `shouldEqual` 7

  it "supersedes immediately while old finalizers overlap and stale effects stay fenced" do
    harness <- makeHarness \_ _ -> pure unit
    Aff.finally (liftEffect $ deactivate harness.runtime) do
      oldWork <- liftEffect makeGate
      oldFinalizer <- liftEffect makeGate
      newWork <- liftEffect makeGate
      externalWitness <- liftEffect $ Ref.new false
      oldLaunched <- liftEffect EffectAVar.empty
      newLaunched <- liftEffect EffectAVar.empty

      liftEffect $ dispatch harness.runtime
        (RunSupersede (CancellableBody oldWork oldFinalizer externalWitness) oldLaunched)
      void $ await "old task launch" oldLaunched
      void $ await "old task body" oldWork.started
      awaitStatus "old task active" Task.Active harness.updates

      liftEffect $ dispatch harness.runtime (RunSupersede (WaitBody newWork (Right 2)) newLaunched)
      void $ await "new superseding launch" newLaunched
      void $ await "old finalizer overlap" oldFinalizer.started
      void $ await "new task during old finalizer" newWork.started
      active <- liftEffect $ statusOf harness
      active `shouldEqual` Task.Active

      release newWork
      awaitStatus "new task success" (Task.Succeeded 2) harness.updates
      externalBeforeRelease <- liftEffect $ Ref.read externalWitness
      externalBeforeRelease `shouldEqual` false
      stateBeforeRelease <- liftEffect $ Ref.read harness.state
      stateBeforeRelease.elsewhere `shouldEqual` 0

      release oldFinalizer
      void $ await "old finalizer settlement" oldFinalizer.settled
      externalAfterRelease <- liftEffect $ Ref.read externalWitness
      externalAfterRelease `shouldEqual` false
      stateAfterRelease <- liftEffect $ Ref.read harness.state
      stateAfterRelease.elsewhere `shouldEqual` 0
      Task.toStatus stateAfterRelease.task `shouldEqual` Task.Succeeded 2

  it "reset publishes Idle immediately and waits for finalizers" do
    harness <- makeHarness \_ _ -> pure unit
    Aff.finally (liftEffect $ deactivate harness.runtime) do
      work <- liftEffect makeGate
      finalizer <- liftEffect makeGate
      externalWitness <- liftEffect $ Ref.new false
      launched <- liftEffect EffectAVar.empty
      resetDone <- liftEffect EffectAVar.empty

      liftEffect $ dispatch harness.runtime
        (RunSupersede (CancellableBody work finalizer externalWitness) launched)
      void $ await "resettable task launch" launched
      void $ await "resettable task body" work.started
      awaitStatus "resettable active" Task.Active harness.updates

      liftEffect $ dispatch harness.runtime (Reset resetDone)
      awaitStatus "reset publishes idle" Task.Idle harness.updates
      void $ await "reset finalizer start" finalizer.started
      completionBeforeFinalizer <- liftEffect $ EffectAVar.tryTake resetDone
      completionBeforeFinalizer `shouldEqual` Nothing

      release finalizer
      void $ await "reset completion" resetDone
      witness <- liftEffect $ Ref.read externalWitness
      witness `shouldEqual` false
      status <- liftEffect $ statusOf harness
      status `shouldEqual` Task.Idle

  it "returns unexpected failures to Idle and routes ForkError" do
    raised <- liftEffect EffectAVar.empty
    errors <- liftEffect $ Ref.new []
    harness <- makeHarness \context error -> do
      let
        label = case context of
          ForkError _ -> "fork"
          _ -> "wrong context"
      Ref.modify_ (_ <> [ label <> ": " <> Exception.message error ]) errors
      void $ EffectAVar.tryPut unit raised
    Aff.finally (liftEffect $ deactivate harness.runtime) do
      gate <- liftEffect makeGate
      launched <- liftEffect EffectAVar.empty
      liftEffect $ dispatch harness.runtime (RunSupersede (UnexpectedBody gate) launched)
      void $ await "unexpected task launch" launched
      void $ await "unexpected task body" gate.started
      awaitStatus "unexpected task active" Task.Active harness.updates
      release gate
      awaitStatus "unexpected task idle" Task.Idle harness.updates
      void $ await "unexpected task onError" raised
      actual <- liftEffect $ Ref.read errors
      actual `shouldEqual` [ "fork: task boom" ]

  it "debounces with a private trailing timer and keeps only the latest result" do
    harness <- makeHarness \_ _ -> pure unit
    Aff.finally (liftEffect $ deactivate harness.runtime) do
      firstTimer <- liftEffect makeTimer
      secondTimer <- liftEffect makeTimer
      firstBody <- liftEffect makeGate
      secondBody <- liftEffect makeGate
      launched <- liftEffect EffectAVar.empty

      liftEffect $ dispatch harness.runtime
        (RunDebounce firstTimer (Milliseconds 50.0) (WaitBody firstBody (Right 1)) launched)
      void $ await "first debounce launch" launched
      void $ await "first debounce timer" firstTimer.gate.started
      awaitStatus "first debounce active" Task.Active harness.updates

      liftEffect $ dispatch harness.runtime
        (RunDebounce secondTimer (Milliseconds 50.0) (WaitBody secondBody (Right 2)) launched)
      void $ await "second debounce timer" secondTimer.gate.started
      void $ await "cancelled first debounce" firstTimer.gate.settled
      active <- liftEffect $ statusOf harness
      active `shouldEqual` Task.Active
      shouldNotHaveStarted firstBody

      release secondTimer.gate
      void $ await "latest debounced body" secondBody.started
      release secondBody
      awaitStatus "latest debounce success" (Task.Succeeded 2) harness.updates
      shouldNotHaveStarted firstBody

  it "normalizes nonpositive debounce duration and reset cancels its timer" do
    harness <- makeHarness \_ _ -> pure unit
    Aff.finally (liftEffect $ deactivate harness.runtime) do
      timer <- liftEffect makeTimer
      body <- liftEffect makeGate
      launched <- liftEffect EffectAVar.empty
      resetDone <- liftEffect EffectAVar.empty

      liftEffect $ dispatch harness.runtime
        (RunDebounce timer (Milliseconds (-10.0)) (WaitBody body (Right 1)) launched)
      observed <- await "normalized debounce duration" timer.duration
      observed `shouldEqual` Milliseconds 0.0
      void $ await "nonpositive debounce timer" timer.gate.started
      awaitStatus "nonpositive debounce active" Task.Active harness.updates
      shouldNotHaveStarted body

      liftEffect $ dispatch harness.runtime (Reset resetDone)
      void $ await "debounce reset" resetDone
      void $ await "debounce timer cancellation" timer.gate.settled
      awaitStatus "debounce reset idle" Task.Idle harness.updates
      shouldNotHaveStarted body

  it "normalizes active state without a cleanup setter and republishes before reactivation work" do
    harness <- makeHarness \_ _ -> pure unit
    first <- liftEffect makeGate
    second <- liftEffect makeGate
    launched <- liftEffect EffectAVar.empty

    liftEffect $ dispatch harness.runtime (RunOnce (WaitBody first (Right 1)) launched)
    void $ await "pre-deactivation task" first.started
    awaitStatus "pre-deactivation active" Task.Active harness.updates
    callsBefore <- liftEffect $ Ref.read harness.setterCalls

    liftEffect $ deactivate harness.runtime
    void $ await "deactivated task cancellation" first.settled
    callsAfterCleanup <- liftEffect $ Ref.read harness.setterCalls
    callsAfterCleanup `shouldEqual` callsBefore

    liftEffect $ activate harness.runtime
    awaitStatus "reactivation idle publication" Task.Idle harness.updates
    liftEffect $ dispatch harness.runtime (RunOnce (WaitBody second (Right 2)) launched)
    void $ await "reactivated once task" second.started
    awaitStatus "reactivated task active" Task.Active harness.updates
    release second
    awaitStatus "reactivated task success" (Task.Succeeded 2) harness.updates

    ignored <- liftEffect makeGate
    liftEffect do
      deactivate harness.runtime
      activate harness.runtime
      dispatch harness.runtime (RunOnce (WaitBody ignored (Right 3)) launched)
    shouldNotHaveStarted ignored
    terminal <- liftEffect $ statusOf harness
    terminal `shouldEqual` Task.Succeeded 2
    liftEffect $ deactivate harness.runtime

  it "uses the latest state setter for managed completion" do
    harness <- makeHarness \_ _ -> pure unit
    Aff.finally (liftEffect $ deactivate harness.runtime) do
      gate <- liftEffect makeGate
      launched <- liftEffect EffectAVar.empty
      newState <- liftEffect $ Ref.new initialState
      liftEffect $ dispatch harness.runtime (RunSupersede (WaitBody gate (Right 7)) launched)
      void $ await "setter task body" gate.started
      awaitStatus "setter task active" Task.Active harness.updates

      liftEffect $ syncSpec harness.runtime identity
        { spec: { handlers, onError: \_ _ -> pure unit }
        , stateUpdate: flip Ref.write newState
        }
      release gate
      void $ await "setter task settlement" gate.settled
      current <- liftEffect $ Ref.read newState
      Task.toStatus current.task `shouldEqual` Task.Succeeded 7
      old <- liftEffect $ Ref.read harness.state
      Task.toStatus old.task `shouldEqual` Task.Active

  snapshotSpec

newtype AppM a = AppM (ReaderT Int Aff a)

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM

runAppM :: Int -> AppM ~> Aff
runAppM environment (AppM computation) = runReaderT computation environment

readEnvironment :: AppM Int
readEnvironment = AppM ask

type SnapshotState = { task :: Task.State String Int }

snapshotLens :: Lens' SnapshotState (Task.State String Int)
snapshotLens = prop (Proxy :: Proxy "task")

data SnapshotAction = LaunchSnapshot Gate Gate (AVar Int)

snapshotHandlers :: Handlers Unit SnapshotState SnapshotAction AppM
snapshotHandlers = defaultHandlers
  { onAction = \(LaunchSnapshot handlerGate bodyGate result) -> do
      lift $ AppM $ lift $ waitForGate handlerGate
      Task.supersede snapshotLens do
        lift $ AppM $ lift $ waitForGate bodyGate
        environment <- lift readEnvironment
        lift $ AppM $ lift $ void $ AVar.tryPut environment result
        pure (Right environment)
  }

snapshotSpec :: Spec Unit
snapshotSpec = describe "managed task interpreter snapshots" do
  it "inherits the launching handler's interpreter" do
    handlerGate <- liftEffect makeGate
    bodyGate <- liftEffect makeGate
    result <- liftEffect EffectAVar.empty
    runtime <- liftEffect $ createRuntime (runAppM 1)
      { initialProps: unit
      , initialState: { task: Task.idle }
      , spec: { handlers: snapshotHandlers, onError: \_ _ -> pure unit }
      , stateUpdate: \_ -> pure unit
      }
    Aff.finally (liftEffect $ deactivate runtime) do
      liftEffect do
        activate runtime
        dispatch runtime (LaunchSnapshot handlerGate bodyGate result)
      void $ await "snapshot handler" handlerGate.started
      liftEffect $ syncSpec runtime (runAppM 2)
        { spec: { handlers: snapshotHandlers, onError: \_ _ -> pure unit }
        , stateUpdate: \_ -> pure unit
        }
      release handlerGate
      void $ await "snapshot task body" bodyGate.started
      release bodyGate
      actual <- await "snapshot task environment" result
      actual `shouldEqual` 1
