module Test.Halo.TaskSpec (spec) where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (get, modify_, put)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Lens (Lens', preview, review)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
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
import React.Halo.Internal.Task.Types as TaskTypes
import React.Halo.Internal.Types (ErrorContext(..))
import React.Halo.Task as Task
import Test.Halo.Helpers (Gate, await, makeGate, release, shouldNotHaveStarted, waitForGate)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

type ComponentState =
  { elsewhere :: Int
  , other :: Task.State String Int
  , task :: Task.State String Int
  }

taskLens :: Lens' ComponentState (Task.State String Int)
taskLens = prop (Proxy :: Proxy "task")

taskSlot :: Task.Slot "task" ComponentState String Int
taskSlot = Task.slot (Proxy :: Proxy "task")

otherLens :: Lens' ComponentState (Task.State String Int)
otherLens = prop (Proxy :: Proxy "other")

otherSlot :: Task.Slot "other" ComponentState String Int
otherSlot = Task.slot (Proxy :: Proxy "other")

sameBrandOtherSlot :: Task.Slot "task" ComponentState String Int
sameBrandOtherSlot = Task.slotAt (Proxy :: Proxy "task") otherLens

differentBrandTaskSlot :: Task.Slot "alias" ComponentState String Int
differentBrandTaskSlot = Task.slotAt (Proxy :: Proxy "alias") taskLens

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
  | RunOther Body (AVar Unit)
  | Reset (AVar Unit)
  | ResetOther (AVar Unit)
  | AssignIdleAndStart Body (AVar Unit)
  | CopyTaskToOther (AVar Unit)
  | CaptureState (AVar ComponentState)
  | RestoreState ComponentState (AVar Unit)
  | RunSameBrandCollision Body
  | RunDifferentBrandCollision Body

type UI a = HaloM Unit ComponentState Action Aff a

handlers :: Handlers Unit ComponentState Action Aff
handlers = defaultHandlers
  { onAction = case _ of
      RunOnce body launched -> do
        Task.once taskSlot (runBody body)
        signal launched
      RunStartIfInactive body launched -> do
        Task.startIfInactive taskSlot (runBody body)
        signal launched
      RunStartTwice first second launched -> do
        Task.startIfInactive taskSlot (runBody first)
        Task.startIfInactive taskSlot (runBody second)
        signal launched
      RunSupersede body launched -> do
        Task.supersede taskSlot (runBody body)
        signal launched
      RunDebounce timer duration body launched -> do
        TaskInternal.debounceWith (runTimer timer) taskSlot duration (runBody body)
        signal launched
      RunOther body launched -> do
        Task.supersede otherSlot (runBody body)
        signal launched
      Reset completed -> do
        Task.reset taskSlot
        signal completed
      ResetOther completed -> do
        Task.reset otherSlot
        signal completed
      AssignIdleAndStart body launched -> do
        modify_ _ { task = Task.idle taskSlot }
        Task.startIfInactive taskSlot (runBody body)
        signal launched
      CopyTaskToOther completed -> do
        modify_ \state -> state { other = state.task }
        signal completed
      CaptureState captured -> get >>= liftAff <<< void <<< flip AVar.tryPut captured
      RestoreState snapshot completed -> do
        put snapshot
        signal completed
      RunSameBrandCollision body -> Task.supersede sameBrandOtherSlot (runBody body)
      RunDifferentBrandCollision body -> Task.supersede differentBrandTaskSlot (runBody body)
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
  { otherUpdates :: AVar (Task.Status String Int)
  , runtime :: Runtime Unit ComponentState Action Aff
  , setterCalls :: Ref.Ref Int
  , state :: Ref.Ref ComponentState
  , tasks :: Ref.Ref (Task.View ComponentState)
  , updates :: AVar (Task.Status String Int)
  }

makeHarness
  :: (ErrorContext Unit Action -> Exception.Error -> Effect Unit)
  -> Aff Harness
makeHarness onError = liftEffect do
  state <- Ref.new initialState
  tasks <- Ref.new (TaskTypes.emptyView initialState)
  updates <- EffectAVar.empty
  otherUpdates <- EffectAVar.empty
  setterCalls <- Ref.new 0
  runtime <- createRuntime identity
    { initialProps: unit
    , initialState
    , spec: { handlers, onError }
    , stateUpdate: updateState state tasks updates otherUpdates setterCalls
    }
  activate runtime
  pure { otherUpdates, runtime, setterCalls, state, tasks, updates }

initialState :: ComponentState
initialState =
  { elsewhere: 0
  , other: Task.idle otherSlot
  , task: Task.idle taskSlot
  }

updateState
  :: Ref.Ref ComponentState
  -> Ref.Ref (Task.View ComponentState)
  -> AVar (Task.Status String Int)
  -> AVar (Task.Status String Int)
  -> Ref.Ref Int
  -> ComponentState
  -> Task.View ComponentState
  -> Effect Unit
updateState state tasks updates otherUpdates setterCalls next nextTasks = do
  previousTasks <- Ref.read tasks
  Ref.write next state
  Ref.write nextTasks tasks
  Ref.modify_ (_ + 1) setterCalls
  let
    previousStatus = Task.toStatus previousTasks taskSlot
    nextStatus = Task.toStatus nextTasks taskSlot
    previousOtherStatus = Task.toStatus previousTasks otherSlot
    nextOtherStatus = Task.toStatus nextTasks otherSlot
  when (previousStatus /= nextStatus) do
    void $ EffectAVar.tryPut nextStatus updates
  when (previousOtherStatus /= nextOtherStatus) do
    void $ EffectAVar.tryPut nextOtherStatus otherUpdates

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
statusOf harness = flip Task.toStatus taskSlot <$> Ref.read harness.tasks

otherStatusOf :: Harness -> Effect (Task.Status String Int)
otherStatusOf harness = flip Task.toStatus otherSlot <$> Ref.read harness.tasks

spec :: Spec Unit
spec = describe "state-focused tasks" do
  it "projects status through helpers and lawful prisms" do
    let tasks = TaskTypes.emptyView initialState
    Task.toStatus tasks taskSlot `shouldEqual` Task.Idle
    Task.toMaybe tasks taskSlot `shouldEqual` Nothing
    Task.isActive tasks taskSlot `shouldEqual` false
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
      statusAfterRelease <- liftEffect $ statusOf harness
      statusAfterRelease `shouldEqual` Task.Succeeded 2

  it "reconciles assigned idle before restart and fences the detached root" do
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
      void $ await "assigned-idle old body" oldWork.started
      awaitStatus "assigned-idle old active" Task.Active harness.updates

      liftEffect $ dispatch harness.runtime
        (AssignIdleAndStart (WaitBody newWork (Right 11)) newLaunched)
      void $ await "assigned-idle restart" newLaunched
      awaitStatus "assigned idle publication" Task.Idle harness.updates
      void $ await "assigned-idle old finalizer" oldFinalizer.started
      void $ await "assigned-idle new body" newWork.started
      restartedStatus <- liftEffect $ statusOf harness
      restartedStatus `shouldEqual` Task.Active
      release newWork
      awaitStatus "assigned-idle new success" (Task.Succeeded 11) harness.updates

      release oldFinalizer
      void $ await "assigned-idle old settlement" oldFinalizer.settled
      witness <- liftEffect $ Ref.read externalWitness
      witness `shouldEqual` false
      state <- liftEffect $ Ref.read harness.state
      state.elsewhere `shouldEqual` 0
      status <- liftEffect $ statusOf harness
      status `shouldEqual` Task.Succeeded 11

  it "normalizes a restored stale snapshot and cancels the displaced authority" do
    harness <- makeHarness \_ _ -> pure unit
    Aff.finally (liftEffect $ deactivate harness.runtime) do
      first <- liftEffect makeGate
      currentWork <- liftEffect makeGate
      currentFinalizer <- liftEffect makeGate
      externalWitness <- liftEffect $ Ref.new false
      launched <- liftEffect EffectAVar.empty
      captured <- liftEffect EffectAVar.empty
      restored <- liftEffect EffectAVar.empty

      liftEffect $ dispatch harness.runtime (RunSupersede (WaitBody first (Right 1)) launched)
      void $ await "snapshot first body" first.started
      awaitStatus "snapshot first active" Task.Active harness.updates
      liftEffect $ dispatch harness.runtime (CaptureState captured)
      snapshot <- await "active whole-state snapshot" captured

      liftEffect $ dispatch harness.runtime
        (RunSupersede (CancellableBody currentWork currentFinalizer externalWitness) launched)
      void $ await "snapshot current body" currentWork.started
      void $ await "snapshot old cancellation" first.settled
      liftEffect $ dispatch harness.runtime (RestoreState snapshot restored)
      void $ await "stale snapshot restoration" restored
      void $ await "restored snapshot displaced finalizer" currentFinalizer.started
      awaitStatus "restored snapshot idle" Task.Idle harness.updates

      release currentFinalizer
      void $ await "displaced current settlement" currentFinalizer.settled
      witness <- liftEffect $ Ref.read externalWitness
      witness `shouldEqual` false
      state <- liftEffect $ Ref.read harness.state
      state.elsewhere `shouldEqual` 0
      status <- liftEffect $ statusOf harness
      status `shouldEqual` Task.Idle

  it "preserves restored branded terminal state while fencing current work" do
    harness <- makeHarness \_ _ -> pure unit
    Aff.finally (liftEffect $ deactivate harness.runtime) do
      completed <- liftEffect makeGate
      currentWork <- liftEffect makeGate
      currentFinalizer <- liftEffect makeGate
      externalWitness <- liftEffect $ Ref.new false
      launched <- liftEffect EffectAVar.empty
      captured <- liftEffect EffectAVar.empty
      restored <- liftEffect EffectAVar.empty

      liftEffect $ dispatch harness.runtime (RunSupersede (WaitBody completed (Right 4)) launched)
      void $ await "terminal snapshot body" completed.started
      awaitStatus "terminal snapshot active" Task.Active harness.updates
      release completed
      awaitStatus "terminal snapshot success" (Task.Succeeded 4) harness.updates
      liftEffect $ dispatch harness.runtime (CaptureState captured)
      terminalSnapshot <- await "terminal whole-state snapshot" captured

      liftEffect $ dispatch harness.runtime
        (RunSupersede (CancellableBody currentWork currentFinalizer externalWitness) launched)
      void $ await "terminal replacement current body" currentWork.started
      awaitStatus "terminal replacement active" Task.Active harness.updates
      liftEffect $ dispatch harness.runtime (RestoreState terminalSnapshot restored)
      void $ await "terminal snapshot restoration" restored
      void $ await "terminal replacement finalizer" currentFinalizer.started
      awaitStatus "restored terminal status" (Task.Succeeded 4) harness.updates

      release currentFinalizer
      void $ await "terminal replacement settlement" currentFinalizer.settled
      witness <- liftEffect $ Ref.read externalWitness
      witness `shouldEqual` false
      state <- liftEffect $ Ref.read harness.state
      state.elsewhere `shouldEqual` 0

  it "keeps same-typed slots isolated when active state is copied" do
    harness <- makeHarness \_ _ -> pure unit
    Aff.finally (liftEffect $ deactivate harness.runtime) do
      taskWork <- liftEffect makeGate
      otherWork <- liftEffect makeGate
      otherAgain <- liftEffect makeGate
      launched <- liftEffect EffectAVar.empty
      copied <- liftEffect EffectAVar.empty
      resetDone <- liftEffect EffectAVar.empty

      liftEffect $ dispatch harness.runtime (RunSupersede (WaitBody taskWork (Right 1)) launched)
      void $ await "source slot body" taskWork.started
      awaitStatus "source slot active" Task.Active harness.updates
      liftEffect $ dispatch harness.runtime (CopyTaskToOther copied)
      void $ await "copy active into unused slot" copied
      copiedView <- liftEffect $ Ref.read harness.tasks
      Task.toStatus copiedView taskSlot `shouldEqual` Task.Active
      Task.toStatus copiedView otherSlot `shouldEqual` Task.Idle

      liftEffect $ dispatch harness.runtime (ResetOther resetDone)
      void $ await "reset copied inactive slot" resetDone
      sourceSettled <- liftEffect $ EffectAVar.tryTake taskWork.settled
      sourceSettled `shouldEqual` Nothing

      liftEffect $ dispatch harness.runtime (RunOther (WaitBody otherWork (Right 2)) launched)
      void $ await "independent other slot" otherWork.started
      awaitStatus "other slot active update" Task.Active harness.otherUpdates
      sourceWhileOther <- liftEffect $ statusOf harness
      otherWhileRunning <- liftEffect $ otherStatusOf harness
      sourceWhileOther `shouldEqual` Task.Active
      otherWhileRunning `shouldEqual` Task.Active
      release otherWork
      awaitStatus "other slot success" (Task.Succeeded 2) harness.otherUpdates

      liftEffect $ dispatch harness.runtime (CopyTaskToOther copied)
      void $ await "copy active after other completion" copied
      awaitStatus "copied completed slot becomes idle" Task.Idle harness.otherUpdates
      sourceAfterCopy <- liftEffect $ statusOf harness
      sourceAfterCopy `shouldEqual` Task.Active

      liftEffect $ dispatch harness.runtime (RunOther (WaitBody otherAgain (Right 3)) launched)
      void $ await "other restart after copied state" otherAgain.started
      awaitStatus "other restart active" Task.Active harness.otherUpdates
      liftEffect $ dispatch harness.runtime (ResetOther resetDone)
      void $ await "other reset after restart" resetDone
      awaitStatus "other reset idle" Task.Idle harness.otherUpdates
      sourceAfterOtherReset <- liftEffect $ statusOf harness
      sourceAfterOtherReset `shouldEqual` Task.Active
      sourceStillRunning <- liftEffect $ EffectAVar.tryTake taskWork.settled
      sourceStillRunning `shouldEqual` Nothing

      release taskWork
      awaitStatus "source slot completion" (Task.Succeeded 1) harness.updates

  it "rejects slot identity collisions before mutation or cancellation" do
    errors <- liftEffect $ Ref.new []
    raised <- liftEffect EffectAVar.empty
    harness <- makeHarness \context error -> do
      let
        prefix = case context of
          ActionError _ -> "action: "
          _ -> "wrong: "
      Ref.modify_ (_ <> [ prefix <> Exception.message error ]) errors
      void $ EffectAVar.tryPut unit raised
    Aff.finally (liftEffect $ deactivate harness.runtime) do
      authoritative <- liftEffect makeGate
      sameBrandBody <- liftEffect makeGate
      differentBrandBody <- liftEffect makeGate
      launched <- liftEffect EffectAVar.empty

      liftEffect $ dispatch harness.runtime
        (RunSupersede (WaitBody authoritative (Right 1)) launched)
      void $ await "collision authoritative body" authoritative.started
      awaitStatus "collision authoritative active" Task.Active harness.updates
      callsBefore <- liftEffect $ Ref.read harness.setterCalls

      liftEffect $ dispatch harness.runtime
        (RunSameBrandCollision (WaitBody sameBrandBody (Right 2)))
      void $ await "same-brand collision" raised
      shouldNotHaveStarted sameBrandBody
      firstStillRunning <- liftEffect $ EffectAVar.tryTake authoritative.settled
      firstStillRunning `shouldEqual` Nothing

      liftEffect $ dispatch harness.runtime
        (RunDifferentBrandCollision (WaitBody differentBrandBody (Right 3)))
      void $ await "different-brand collision" raised
      shouldNotHaveStarted differentBrandBody
      secondStillRunning <- liftEffect $ EffectAVar.tryTake authoritative.settled
      secondStillRunning `shouldEqual` Nothing
      status <- liftEffect $ statusOf harness
      status `shouldEqual` Task.Active
      callsAfter <- liftEffect $ Ref.read harness.setterCalls
      callsAfter `shouldEqual` callsBefore

      actual <- liftEffect $ Ref.read errors
      actual `shouldEqual`
        [ "action: Halo task slot \"task\" is already bound to a different state focus"
        , "action: Halo task slot \"alias\" overlaps state focus bound as \"task\""
        ]
      release authoritative
      awaitStatus "collision authority completion" (Task.Succeeded 1) harness.updates

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

  it "publishes two-slot normalization before StrictMode onActivate work" do
    firstTask <- liftEffect makeGate
    firstOther <- liftEffect makeGate
    nextTask <- liftEffect makeGate
    nextOther <- liftEffect makeGate
    taskGate <- liftEffect $ Ref.new firstTask
    otherGate <- liftEffect $ Ref.new firstOther
    snapshots <- liftEffect $ Ref.new []
    setterCalls <- liftEffect $ Ref.new 0
    let
      activationHandlers = defaultHandlers
        { onActivate = do
            currentTask <- liftEffect $ Ref.read taskGate
            currentOther <- liftEffect $ Ref.read otherGate
            Task.once taskSlot do
              liftAff $ waitForGate currentTask
              pure (Right 1)
            Task.once otherSlot do
              liftAff $ waitForGate currentOther
              pure (Right 2)
        }
      stateUpdate _ taskView = do
        Ref.modify_ (_ + 1) setterCalls
        Ref.modify_
          ( _ <>
              [ Tuple
                  (Task.toStatus taskView taskSlot)
                  (Task.toStatus taskView otherSlot)
              ]
          )
          snapshots
    runtime <- liftEffect $ createRuntime identity
      { initialProps: unit
      , initialState
      , spec: { handlers: activationHandlers, onError: \_ _ -> pure unit }
      , stateUpdate
      }

    liftEffect $ activate runtime
    void $ await "first StrictMode task" firstTask.started
    void $ await "first StrictMode other" firstOther.started
    beforeCleanup <- liftEffect $ Ref.read snapshots
    beforeCleanup `shouldEqual`
      [ Tuple Task.Active Task.Idle
      , Tuple Task.Active Task.Active
      ]
    callsBeforeCleanup <- liftEffect $ Ref.read setterCalls

    liftEffect $ deactivate runtime
    void $ await "first StrictMode task cancellation" firstTask.settled
    void $ await "first StrictMode other cancellation" firstOther.settled
    callsAfterCleanup <- liftEffect $ Ref.read setterCalls
    callsAfterCleanup `shouldEqual` callsBeforeCleanup

    liftEffect do
      Ref.write nextTask taskGate
      Ref.write nextOther otherGate
      activate runtime
    void $ await "replayed StrictMode task" nextTask.started
    void $ await "replayed StrictMode other" nextOther.started
    afterReplay <- liftEffect $ Ref.read snapshots
    afterReplay `shouldEqual`
      [ Tuple Task.Active Task.Idle
      , Tuple Task.Active Task.Active
      , Tuple Task.Idle Task.Idle
      , Tuple Task.Active Task.Idle
      , Tuple Task.Active Task.Active
      ]
    liftEffect $ deactivate runtime

  it "normalizes active state without a cleanup setter and republishes before reactivation work" do
    harness <- makeHarness \_ _ -> pure unit
    first <- liftEffect makeGate
    other <- liftEffect makeGate
    second <- liftEffect makeGate
    launched <- liftEffect EffectAVar.empty

    liftEffect $ dispatch harness.runtime (RunOnce (WaitBody first (Right 1)) launched)
    void $ await "pre-deactivation task" first.started
    awaitStatus "pre-deactivation active" Task.Active harness.updates
    liftEffect $ dispatch harness.runtime (RunOther (WaitBody other (Right 9)) launched)
    void $ await "pre-deactivation other task" other.started
    awaitStatus "pre-deactivation other active" Task.Active harness.otherUpdates
    callsBefore <- liftEffect $ Ref.read harness.setterCalls

    liftEffect $ deactivate harness.runtime
    void $ await "deactivated task cancellation" first.settled
    void $ await "deactivated other cancellation" other.settled
    callsAfterCleanup <- liftEffect $ Ref.read harness.setterCalls
    callsAfterCleanup `shouldEqual` callsBefore

    liftEffect $ activate harness.runtime
    awaitStatus "reactivation idle publication" Task.Idle harness.updates
    awaitStatus "reactivation other idle publication" Task.Idle harness.otherUpdates
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

  it "projects a cross-runtime active snapshot Idle on first render" do
    source <- makeHarness \_ _ -> pure unit
    sourceWork <- liftEffect makeGate
    launched <- liftEffect EffectAVar.empty
    liftEffect $ dispatch source.runtime (RunSupersede (WaitBody sourceWork (Right 1)) launched)
    void $ await "cross-runtime source body" sourceWork.started
    awaitStatus "cross-runtime source active" Task.Active source.updates
    foreignState <- liftEffect $ Ref.read source.state

    let firstView = TaskTypes.emptyView foreignState
    Task.toStatus firstView taskSlot `shouldEqual` Task.Idle
    targetState <- liftEffect $ Ref.new foreignState
    targetTasks <- liftEffect $ Ref.new firstView
    target <- liftEffect $ createRuntime identity
      { initialProps: unit
      , initialState: foreignState
      , spec: { handlers, onError: \_ _ -> pure unit }
      , stateUpdate: \state tasks -> do
          Ref.write state targetState
          Ref.write tasks targetTasks
      }
    targetWork <- liftEffect makeGate
    Aff.finally
      ( liftEffect do
          deactivate target
          deactivate source.runtime
      )
      do
        liftEffect do
          activate target
          dispatch target (RunOnce (WaitBody targetWork (Right 2)) launched)
        void $ await "cross-runtime target body" targetWork.started
        sourceSettled <- liftEffect $ EffectAVar.tryTake sourceWork.settled
        sourceSettled `shouldEqual` Nothing
        currentTargetTasks <- liftEffect $ Ref.read targetTasks
        Task.toStatus currentTargetTasks taskSlot `shouldEqual` Task.Active
        release targetWork
        release sourceWork

  it "uses the latest state setter for managed completion" do
    harness <- makeHarness \_ _ -> pure unit
    Aff.finally (liftEffect $ deactivate harness.runtime) do
      gate <- liftEffect makeGate
      launched <- liftEffect EffectAVar.empty
      newState <- liftEffect $ Ref.new initialState
      newTasks <- liftEffect $ Ref.new (TaskTypes.emptyView initialState)
      liftEffect $ dispatch harness.runtime (RunSupersede (WaitBody gate (Right 7)) launched)
      void $ await "setter task body" gate.started
      awaitStatus "setter task active" Task.Active harness.updates

      liftEffect $ syncSpec harness.runtime identity
        { spec: { handlers, onError: \_ _ -> pure unit }
        , stateUpdate: \next tasks -> do
            Ref.write next newState
            Ref.write tasks newTasks
        }
      release gate
      void $ await "setter task settlement" gate.settled
      currentTasks <- liftEffect $ Ref.read newTasks
      Task.toStatus currentTasks taskSlot `shouldEqual` Task.Succeeded 7
      oldTasks <- liftEffect $ Ref.read harness.tasks
      Task.toStatus oldTasks taskSlot `shouldEqual` Task.Active

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

snapshotSlot :: Task.Slot "task" SnapshotState String Int
snapshotSlot = Task.slot (Proxy :: Proxy "task")

data SnapshotAction = LaunchSnapshot Gate Gate (AVar Int)

snapshotHandlers :: Handlers Unit SnapshotState SnapshotAction AppM
snapshotHandlers = defaultHandlers
  { onAction = \(LaunchSnapshot handlerGate bodyGate result) -> do
      lift $ AppM $ lift $ waitForGate handlerGate
      Task.supersede snapshotSlot do
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
      , initialState: { task: Task.idle snapshotSlot }
      , spec: { handlers: snapshotHandlers, onError: \_ _ -> pure unit }
      , stateUpdate: \_ _ -> pure unit
      }
    Aff.finally (liftEffect $ deactivate runtime) do
      liftEffect do
        activate runtime
        dispatch runtime (LaunchSnapshot handlerGate bodyGate result)
      void $ await "snapshot handler" handlerGate.started
      liftEffect $ syncSpec runtime (runAppM 2)
        { spec: { handlers: snapshotHandlers, onError: \_ _ -> pure unit }
        , stateUpdate: \_ _ -> pure unit
        }
      release handlerGate
      void $ await "snapshot task body" bodyGate.started
      release bodyGate
      actual <- await "snapshot task environment" result
      actual `shouldEqual` 1
