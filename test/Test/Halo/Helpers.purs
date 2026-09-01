module Test.Halo.Helpers
  ( Gate
  , await
  , makeGate
  , release
  , shouldNotHaveStarted
  , waitForGate
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Parallel (parallel, sequential)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.AVar as AVar
import Effect.AVar (AVar)
import Effect.AVar as EffectAVar
import Test.Spec.Assertions (shouldEqual)

type Gate =
  { release :: AVar Unit
  , settled :: AVar Unit
  , started :: AVar Unit
  }

makeGate :: Effect Gate
makeGate = do
  started <- EffectAVar.empty
  releaseGate <- EffectAVar.empty
  settled <- EffectAVar.empty
  pure { started, release: releaseGate, settled }

waitForGate :: Gate -> Aff Unit
waitForGate gate = Aff.finally
  (void $ AVar.tryPut unit gate.settled)
  do
    AVar.put unit gate.started
    void $ AVar.take gate.release

await :: forall a. String -> AVar a -> Aff a
await label value =
  sequential $
    parallel (AVar.take value)
      <|> parallel (Aff.delay (Milliseconds 2_000.0) *> Aff.throwError (Aff.error ("Timed out waiting for " <> label)))

release :: Gate -> Aff Unit
release = AVar.put unit <<< _.release

shouldNotHaveStarted :: Gate -> Aff Unit
shouldNotHaveStarted gate = do
  started <- AVar.tryTake gate.started
  started `shouldEqual` Nothing
