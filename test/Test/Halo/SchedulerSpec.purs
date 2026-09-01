module Test.Halo.SchedulerSpec (spec) where

import Prelude

import Data.Array as Array
import Effect.AVar as EffectAVar
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import React.Halo as Halo
import React.Halo.Internal.Runtime (activate, deactivate, dispatch)
import Test.Halo.Helpers (Action(..), Key(..), await, awaitCounts, makeGate, release, runGate, shouldNotHaveStarted, withHarness, work)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "first-class task scheduling" do
  it "handles an action immediately without counting it as task activity" $ withHarness \harness -> do
    gate <- liftEffect makeGate
    liftEffect $ dispatch harness.runtime (Direct 1 gate)
    void $ await "direct action start" gate.started

    awaitCounts harness { running: 0, queued: 0 }
    release gate
    void $ await "direct action completion" gate.settled
    state <- liftEffect $ Ref.read harness.state
    state `shouldEqual` [ 1 ]

  it "keeps performed work alive after its launching handler completes" $ withHarness \harness -> do
    gate <- liftEffect makeGate
    witness <- liftEffect makeGate
    let task = Halo.concurrent Search work
    liftEffect $ dispatch harness.runtime (PerformWithWitness task { value: 1, gate } witness)

    void $ await "task submission" gate.launched
    void $ await "launching handler completion" witness.settled
    void $ await "performed task start" gate.started
    awaitCounts harness { running: 1, queued: 0 }

    release gate
    void $ await "performed task completion" gate.settled
    awaitCounts harness { running: 0, queued: 0 }

  it "concurrent preserves inputs and runs same-key performances together" $ withHarness \harness -> do
    first <- liftEffect makeGate
    second <- liftEffect makeGate
    let
      task = Halo.concurrent Search work
      idleTask = Halo.concurrent Save work

    liftEffect do
      dispatch harness.runtime (Perform task { value: 1, gate: first })
      dispatch harness.runtime (Perform task { value: 2, gate: second })

    void $ await "first concurrent task start" first.started
    void $ await "second concurrent task start" second.started
    awaitCounts harness { running: 2, queued: 0 }
    activity <- liftEffect $ Ref.read harness.activity
    Halo.activity task activity `shouldEqual` { running: 2, queued: 0 }
    Halo.activity idleTask activity `shouldEqual` { running: 0, queued: 0 }

    release second
    release first
    void $ await "first concurrent task completion" first.settled
    void $ await "second concurrent task completion" second.settled
    state <- liftEffect $ Ref.read harness.state
    Array.sort state `shouldEqual` [ 1, 2 ]

  it "perform_ submits a unit-input task" $ withHarness \harness -> do
    gate <- liftEffect makeGate
    let task = Halo.concurrent Save \_ -> runGate 7 gate
    liftEffect $ dispatch harness.runtime (PerformUnit task gate)

    void $ await "unit task start" gate.started
    release gate
    void $ await "unit task completion" gate.settled
    state <- liftEffect $ Ref.read harness.state
    state `shouldEqual` [ 7 ]

  it "restartable cancels and commit-fences previous work" $ withHarness \harness -> do
    stale <- liftEffect makeGate
    current <- liftEffect makeGate
    let task = Halo.restartable Search work

    liftEffect $ dispatch harness.runtime (Perform task { value: 1, gate: stale })
    void $ await "stale restartable task start" stale.started
    liftEffect $ dispatch harness.runtime (Perform task { value: 2, gate: current })

    void $ await "stale restartable task cancellation" stale.settled
    void $ await "replacement restartable task start" current.started
    release current
    void $ await "replacement restartable task completion" current.settled
    state <- liftEffect $ Ref.read harness.state
    state `shouldEqual` [ 2 ]

  it "drop ignores new input while its key is busy" $ withHarness \harness -> do
    running <- liftEffect makeGate
    dropped <- liftEffect makeGate
    let task = Halo.drop Save work

    liftEffect $ dispatch harness.runtime (Perform task { value: 1, gate: running })
    void $ await "drop task start" running.started
    liftEffect $ dispatch harness.runtime (Perform task { value: 2, gate: dropped })
    void $ await "dropped submission" dropped.launched
    awaitCounts harness { running: 1, queued: 0 }
    shouldNotHaveStarted dropped

    release running
    void $ await "drop task completion" running.settled
    shouldNotHaveStarted dropped
    state <- liftEffect $ Ref.read harness.state
    state `shouldEqual` [ 1 ]

  it "enqueue preserves every queued payload FIFO" $ withHarness \harness -> do
    first <- liftEffect makeGate
    second <- liftEffect makeGate
    third <- liftEffect makeGate
    let task = Halo.enqueue Save work

    liftEffect do
      dispatch harness.runtime (Perform task { value: 1, gate: first })
      dispatch harness.runtime (Perform task { value: 2, gate: second })
      dispatch harness.runtime (Perform task { value: 3, gate: third })

    void $ await "first enqueue task start" first.started
    awaitCounts harness { running: 1, queued: 2 }
    shouldNotHaveStarted second
    shouldNotHaveStarted third

    release first
    void $ await "second enqueue task start" second.started
    release second
    void $ await "third enqueue task start" third.started
    release third
    void $ await "third enqueue task completion" third.settled
    state <- liftEffect $ Ref.read harness.state
    state `shouldEqual` [ 1, 2, 3 ]

  it "keepLatest retains only the newest queued payload" $ withHarness \harness -> do
    first <- liftEffect makeGate
    discarded <- liftEffect makeGate
    latest <- liftEffect makeGate
    let task = Halo.keepLatest Search work

    liftEffect do
      dispatch harness.runtime (Perform task { value: 1, gate: first })
      dispatch harness.runtime (Perform task { value: 2, gate: discarded })
      dispatch harness.runtime (Perform task { value: 3, gate: latest })

    void $ await "current keepLatest task start" first.started
    awaitCounts harness { running: 1, queued: 1 }
    release first
    void $ await "latest keepLatest task start" latest.started
    shouldNotHaveStarted discarded
    release latest
    void $ await "latest keepLatest task completion" latest.settled
    state <- liftEffect $ Ref.read harness.state
    state `shouldEqual` [ 1, 3 ]

  it "cancel uses task identity to cancel running and queued work" $ withHarness \harness -> do
    running <- liftEffect makeGate
    queued <- liftEffect makeGate
    cancelled <- liftEffect EffectAVar.empty
    let task = Halo.enqueue Search work

    liftEffect do
      dispatch harness.runtime (Perform task { value: 1, gate: running })
      dispatch harness.runtime (Perform task { value: 2, gate: queued })
    void $ await "task before cancellation" running.started
    awaitCounts harness { running: 1, queued: 1 }

    liftEffect $ dispatch harness.runtime (Cancel task cancelled)
    void $ await "task cancellation action" cancelled
    void $ await "running task cancellation" running.settled
    awaitCounts harness { running: 0, queued: 0 }
    shouldNotHaveStarted queued

  it "same-key definitions with the same strategy intentionally share a slot" $ withHarness \harness -> do
    first <- liftEffect makeGate
    second <- liftEffect makeGate
    let
      firstDefinition = Halo.enqueue Search work
      secondDefinition = Halo.enqueue Search work

    liftEffect do
      dispatch harness.runtime (Perform firstDefinition { value: 1, gate: first })
      dispatch harness.runtime (Perform secondDefinition { value: 2, gate: second })
    void $ await "shared slot first task" first.started
    awaitCounts harness { running: 1, queued: 1 }
    activity <- liftEffect $ Ref.read harness.activity
    Halo.activity firstDefinition activity `shouldEqual` Halo.activity secondDefinition activity
    release first
    void $ await "shared slot second task" second.started
    release second
    void $ await "shared slot completion" second.settled

  it "rejects a conflicting same-key strategy through onError" $ withHarness \harness -> do
    running <- liftEffect makeGate
    rejected <- liftEffect makeGate
    let
      established = Halo.enqueue Search work
      conflicting = Halo.restartable Search work

    liftEffect do
      dispatch harness.runtime (Perform established { value: 1, gate: running })
      dispatch harness.runtime (Perform conflicting { value: 2, gate: rejected })
    void $ await "established task start" running.started
    void $ await "configuration error" harness.errorRaised
    shouldNotHaveStarted rejected
    errors <- liftEffect $ Ref.read harness.errors
    errors `shouldEqual`
      [ "task configuration Search: Task key was already defined as enqueue and cannot also be defined as restartable" ]
    release running

  it "remembers a key's strategy across deactivate and reactivate" $ withHarness \harness -> do
    first <- liftEffect makeGate
    conflictingGate <- liftEffect makeGate
    let
      established = Halo.concurrent Search work
      conflicting = Halo.drop Search work

    liftEffect $ dispatch harness.runtime (Perform established { value: 1, gate: first })
    void $ await "task before deactivation" first.started
    liftEffect $ deactivate harness.runtime
    void $ await "deactivated task cancellation" first.settled
    liftEffect do
      activate harness.runtime
      dispatch harness.runtime (Perform conflicting { value: 2, gate: conflictingGate })
    void $ await "remembered configuration error" harness.errorRaised
    shouldNotHaveStarted conflictingGate
