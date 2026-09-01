module Test.Halo.SchedulerSpec (spec) where

import Prelude

import Data.Array as Array
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import React.Halo.Internal.Runtime (dispatch)
import React.Halo.Internal.Types (TaskPolicy(..), activityFor)
import Test.Halo.Helpers (Action(..), Key(..), await, awaitCounts, makeGate, release, shouldNotHaveStarted, withHarness)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "action scheduling" do
  it "Every runs every action concurrently" $ withHarness \harness -> do
    first <- liftEffect makeGate
    second <- liftEffect makeGate

    liftEffect do
      dispatch harness.runtime (Work Every 1 first)
      dispatch harness.runtime (Work Every 2 second)

    void $ await "first Every action start" first.started
    void $ await "second Every action start" second.started
    awaitCounts harness { running: 2, queued: 0 }

    release second
    release first
    void $ await "first Every action completion" first.settled
    void $ await "second Every action completion" second.settled
    awaitCounts harness { running: 0, queued: 0 }

    state <- liftEffect $ Ref.read harness.state
    Array.sort state `shouldEqual` [ 1, 2 ]

  it "Restartable cancels and commit-fences the previous keyed action" $ withHarness \harness -> do
    stale <- liftEffect makeGate
    current <- liftEffect makeGate

    liftEffect $ dispatch harness.runtime (Work (Restartable Search) 1 stale)
    void $ await "stale Restartable action start" stale.started
    liftEffect $ dispatch harness.runtime (Work (Restartable Search) 2 current)

    void $ await "stale Restartable action cancellation" stale.settled
    void $ await "replacement Restartable action start" current.started
    awaitCounts harness { running: 1, queued: 0 }

    release current
    void $ await "replacement Restartable action completion" current.settled
    awaitCounts harness { running: 0, queued: 0 }

    state <- liftEffect $ Ref.read harness.state
    state `shouldEqual` [ 2 ]

  it "Drop ignores new work while the key is running" $ withHarness \harness -> do
    running <- liftEffect makeGate
    dropped <- liftEffect makeGate

    liftEffect $ dispatch harness.runtime (Work (Drop Save) 1 running)
    void $ await "Drop action start" running.started
    liftEffect $ dispatch harness.runtime (Work (Drop Save) 2 dropped)
    awaitCounts harness { running: 1, queued: 0 }
    shouldNotHaveStarted dropped

    release running
    void $ await "Drop action completion" running.settled
    awaitCounts harness { running: 0, queued: 0 }
    shouldNotHaveStarted dropped

    state <- liftEffect $ Ref.read harness.state
    state `shouldEqual` [ 1 ]

  it "Enqueue runs all keyed actions FIFO, one at a time" $ withHarness \harness -> do
    first <- liftEffect makeGate
    second <- liftEffect makeGate
    third <- liftEffect makeGate

    liftEffect do
      dispatch harness.runtime (Work (Enqueue Save) 1 first)
      dispatch harness.runtime (Work (Enqueue Save) 2 second)
      dispatch harness.runtime (Work (Enqueue Save) 3 third)

    void $ await "first Enqueue action start" first.started
    awaitCounts harness { running: 1, queued: 2 }
    shouldNotHaveStarted second
    shouldNotHaveStarted third

    release first
    void $ await "second Enqueue action start" second.started
    awaitCounts harness { running: 1, queued: 1 }
    release second
    void $ await "third Enqueue action start" third.started
    awaitCounts harness { running: 1, queued: 0 }
    release third
    void $ await "third Enqueue action completion" third.settled
    awaitCounts harness { running: 0, queued: 0 }

    state <- liftEffect $ Ref.read harness.state
    state `shouldEqual` [ 1, 2, 3 ]

  it "KeepLatest finishes current work and retains only the newest queued action" $ withHarness \harness -> do
    first <- liftEffect makeGate
    discarded <- liftEffect makeGate
    latest <- liftEffect makeGate

    liftEffect do
      dispatch harness.runtime (Work (KeepLatest Search) 1 first)
      dispatch harness.runtime (Work (KeepLatest Search) 2 discarded)
      dispatch harness.runtime (Work (KeepLatest Search) 3 latest)

    void $ await "current KeepLatest action start" first.started
    awaitCounts harness { running: 1, queued: 1 }
    shouldNotHaveStarted discarded
    shouldNotHaveStarted latest

    release first
    void $ await "latest KeepLatest action start" latest.started
    shouldNotHaveStarted discarded
    release latest
    void $ await "latest KeepLatest action completion" latest.settled
    awaitCounts harness { running: 0, queued: 0 }

    state <- liftEffect $ Ref.read harness.state
    state `shouldEqual` [ 1, 3 ]

  it "reports keyed running and queued activity for rendering" $ withHarness \harness -> do
    running <- liftEffect makeGate
    queued <- liftEffect makeGate

    liftEffect do
      dispatch harness.runtime (Work (Enqueue Search) 1 running)
      dispatch harness.runtime (Work (Enqueue Search) 2 queued)
    void $ await "keyed activity action start" running.started
    awaitCounts harness { running: 1, queued: 1 }

    activity <- liftEffect $ Ref.read harness.activity
    activityFor Search activity `shouldEqual` { running: 1, queued: 1 }
    activityFor Save activity `shouldEqual` { running: 0, queued: 0 }

    release running
    void $ await "queued keyed activity action start" queued.started
    release queued
    void $ await "queued keyed activity action completion" queued.settled
    awaitCounts harness { running: 0, queued: 0 }
