module Test.Halo.SchedulerSpec (spec) where

import Prelude

import Data.Array as Array
import Effect.AVar as EffectAVar
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import React.Halo.Internal.Runtime (dispatch)
import React.Halo.Internal.Types (TaskPolicy(..), activityFor)
import Test.Halo.Helpers (Action(..), Key(..), await, awaitCounts, makeGate, release, shouldNotHaveStarted, withHarness)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "explicit task scheduling" do
  it "handles an action immediately without counting it as task activity" $ withHarness \harness -> do
    gate <- liftEffect makeGate
    liftEffect $ dispatch harness.runtime (Direct 1 gate)
    void $ await "direct action start" gate.started

    awaitCounts harness { running: 0, queued: 0 }
    release gate
    void $ await "direct action completion" gate.settled
    state <- liftEffect $ Ref.read harness.state
    state `shouldEqual` [ 1 ]

  it "keeps a submitted task alive after its launching action completes" $ withHarness \harness -> do
    gate <- liftEffect makeGate
    witness <- liftEffect makeGate
    liftEffect $ dispatch harness.runtime (StartTaskWithWitness Every 1 gate witness)

    void $ await "task submission" gate.launched
    void $ await "launching handler completion" witness.settled
    void $ await "submitted task start" gate.started
    awaitCounts harness { running: 1, queued: 0 }

    release gate
    void $ await "submitted task completion" gate.settled
    awaitCounts harness { running: 0, queued: 0 }

  it "Every runs every submitted task concurrently" $ withHarness \harness -> do
    first <- liftEffect makeGate
    second <- liftEffect makeGate

    liftEffect do
      dispatch harness.runtime (StartTask Every 1 first)
      dispatch harness.runtime (StartTask Every 2 second)

    void $ await "first Every task launch" first.launched
    void $ await "second Every task launch" second.launched
    void $ await "first Every task start" first.started
    void $ await "second Every task start" second.started
    awaitCounts harness { running: 2, queued: 0 }

    release second
    release first
    void $ await "first Every task completion" first.settled
    void $ await "second Every task completion" second.settled
    awaitCounts harness { running: 0, queued: 0 }

    state <- liftEffect $ Ref.read harness.state
    Array.sort state `shouldEqual` [ 1, 2 ]

  it "Restartable cancels and commit-fences the previous keyed task" $ withHarness \harness -> do
    stale <- liftEffect makeGate
    current <- liftEffect makeGate

    liftEffect $ dispatch harness.runtime (StartTask (Restartable Search) 1 stale)
    void $ await "stale Restartable task start" stale.started
    liftEffect $ dispatch harness.runtime (StartTask (Restartable Search) 2 current)

    void $ await "stale Restartable task cancellation" stale.settled
    void $ await "replacement Restartable task start" current.started
    awaitCounts harness { running: 1, queued: 0 }

    release current
    void $ await "replacement Restartable task completion" current.settled
    awaitCounts harness { running: 0, queued: 0 }

    state <- liftEffect $ Ref.read harness.state
    state `shouldEqual` [ 2 ]

  it "Drop ignores a new task while the key is busy" $ withHarness \harness -> do
    running <- liftEffect makeGate
    dropped <- liftEffect makeGate

    liftEffect $ dispatch harness.runtime (StartTask (Drop Save) 1 running)
    void $ await "Drop task start" running.started
    liftEffect $ dispatch harness.runtime (StartTask (Drop Save) 2 dropped)
    void $ await "dropped task launching action completion" dropped.launched
    awaitCounts harness { running: 1, queued: 0 }
    shouldNotHaveStarted dropped

    release running
    void $ await "Drop task completion" running.settled
    awaitCounts harness { running: 0, queued: 0 }
    shouldNotHaveStarted dropped

    state <- liftEffect $ Ref.read harness.state
    state `shouldEqual` [ 1 ]

  it "Enqueue runs all keyed tasks FIFO, one at a time" $ withHarness \harness -> do
    first <- liftEffect makeGate
    second <- liftEffect makeGate
    third <- liftEffect makeGate

    liftEffect do
      dispatch harness.runtime (StartTask (Enqueue Save) 1 first)
      dispatch harness.runtime (StartTask (Enqueue Save) 2 second)
      dispatch harness.runtime (StartTask (Enqueue Save) 3 third)

    void $ await "first Enqueue task start" first.started
    void $ await "second Enqueue task submission" second.launched
    void $ await "third Enqueue task submission" third.launched
    awaitCounts harness { running: 1, queued: 2 }
    shouldNotHaveStarted second
    shouldNotHaveStarted third

    release first
    void $ await "second Enqueue task start" second.started
    awaitCounts harness { running: 1, queued: 1 }
    release second
    void $ await "third Enqueue task start" third.started
    awaitCounts harness { running: 1, queued: 0 }
    release third
    void $ await "third Enqueue task completion" third.settled
    awaitCounts harness { running: 0, queued: 0 }

    state <- liftEffect $ Ref.read harness.state
    state `shouldEqual` [ 1, 2, 3 ]

  it "KeepLatest retains only the newest queued task" $ withHarness \harness -> do
    first <- liftEffect makeGate
    discarded <- liftEffect makeGate
    latest <- liftEffect makeGate

    liftEffect do
      dispatch harness.runtime (StartTask (KeepLatest Search) 1 first)
      dispatch harness.runtime (StartTask (KeepLatest Search) 2 discarded)
      dispatch harness.runtime (StartTask (KeepLatest Search) 3 latest)

    void $ await "current KeepLatest task start" first.started
    void $ await "discarded task submission" discarded.launched
    void $ await "latest task submission" latest.launched
    awaitCounts harness { running: 1, queued: 1 }
    shouldNotHaveStarted discarded
    shouldNotHaveStarted latest

    release first
    void $ await "latest KeepLatest task start" latest.started
    shouldNotHaveStarted discarded
    release latest
    void $ await "latest KeepLatest task completion" latest.settled
    awaitCounts harness { running: 0, queued: 0 }

    state <- liftEffect $ Ref.read harness.state
    state `shouldEqual` [ 1, 3 ]

  it "cancels running and queued tasks for a key" $ withHarness \harness -> do
    running <- liftEffect makeGate
    queued <- liftEffect makeGate
    cancelled <- liftEffect EffectAVar.empty

    liftEffect do
      dispatch harness.runtime (StartTask (Enqueue Search) 1 running)
      dispatch harness.runtime (StartTask (Enqueue Search) 2 queued)
    void $ await "task before keyed cancellation" running.started
    void $ await "queued task submission" queued.launched
    awaitCounts harness { running: 1, queued: 1 }

    liftEffect $ dispatch harness.runtime (CancelTask Search cancelled)
    void $ await "keyed cancellation action" cancelled
    void $ await "running keyed task cancellation" running.settled
    awaitCounts harness { running: 0, queued: 0 }
    shouldNotHaveStarted queued

    state <- liftEffect $ Ref.read harness.state
    state `shouldEqual` []

  it "reports explicit keyed task activity for rendering" $ withHarness \harness -> do
    running <- liftEffect makeGate
    queued <- liftEffect makeGate

    liftEffect do
      dispatch harness.runtime (StartTask (Enqueue Search) 1 running)
      dispatch harness.runtime (StartTask (Enqueue Search) 2 queued)
    void $ await "keyed activity task start" running.started
    awaitCounts harness { running: 1, queued: 1 }

    activity <- liftEffect $ Ref.read harness.activity
    activityFor Search activity `shouldEqual` { running: 1, queued: 1 }
    activityFor Save activity `shouldEqual` { running: 0, queued: 0 }

    release running
    void $ await "queued keyed activity task start" queued.started
    release queued
    void $ await "queued keyed activity task completion" queued.settled
    awaitCounts harness { running: 0, queued: 0 }
