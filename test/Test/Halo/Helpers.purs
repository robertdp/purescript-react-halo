module Test.Halo.Helpers
  ( Action(..)
  , Gate
  , Harness
  , Key(..)
  , await
  , awaitCounts
  , makeGate
  , makeHarness
  , policyOf
  , release
  , runAction
  , shouldNotHaveStarted
  , withHarness
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.State (modify_)
import Control.Parallel (parallel, sequential)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (message)
import Effect.AVar (AVar)
import Effect.AVar as EffectAVar
import Effect.Ref (Ref)
import Effect.Ref as Ref
import React.Halo.Internal.Runtime (HaloM, Runtime, activate, createRuntime, deactivate)
import React.Halo.Internal.Types (Activity, ErrorContext(..), Lifecycle(..), TaskCounts, TaskPolicy(..), activityTotals, emptyActivity)
import Test.Spec.Assertions (fail, shouldEqual)

data Key = Search | Save

derive instance eqKey :: Eq Key
derive instance ordKey :: Ord Key
instance showKey :: Show Key where
  show Search = "Search"
  show Save = "Save"

type Gate =
  { release :: AVar Unit
  , settled :: AVar Unit
  , started :: AVar Unit
  }

data Action
  = Work (TaskPolicy Key) Int Gate
  | Boom Gate

type Harness =
  { activity :: Ref (Activity Key)
  , activityChanged :: AVar Unit
  , errors :: Ref (Array String)
  , errorRaised :: AVar Unit
  , runtime :: Runtime Unit (Array Int) Action Key
  , state :: Ref (Array Int)
  }

makeGate :: Effect Gate
makeGate = do
  started <- EffectAVar.empty
  releaseGate <- EffectAVar.empty
  settled <- EffectAVar.empty
  pure { started, release: releaseGate, settled }

policyOf :: Action -> TaskPolicy Key
policyOf = case _ of
  Work policy _ _ -> policy
  Boom _ -> Every

runAction :: Lifecycle Unit Action -> HaloM Unit (Array Int) Action Key Unit
runAction = case _ of
  Action (Work _ value gate) -> do
    liftAff $ Aff.finally
      (void $ AVar.tryPut unit gate.settled)
      do
        AVar.put unit gate.started
        void $ AVar.take gate.release
    modify_ (flip Array.snoc value)
  Action (Boom gate) ->
    liftAff $ Aff.finally
      (void $ AVar.tryPut unit gate.settled)
      (Aff.throwError (Aff.error "boom"))
  Activate -> pure unit
  Update _ -> pure unit

makeHarness :: Aff Harness
makeHarness = liftEffect do
  activity <- Ref.new emptyActivity
  activityChanged <- EffectAVar.empty
  errors <- Ref.new []
  errorRaised <- EffectAVar.empty
  state <- Ref.new []
  runtime <- createRuntime
    { activityUpdate: \next -> do
        Ref.write next activity
        void $ EffectAVar.tryPut unit activityChanged
    , initialProps: unit
    , initialState: []
    , spec:
        { eval: runAction
        , onError: \context error -> do
            Ref.modify_ (\current -> Array.snoc current (contextName context <> ": " <> message error)) errors
            void $ EffectAVar.tryPut unit errorRaised
        , schedule: policyOf
        }
    , stateUpdate: flip Ref.write state
    }
  activate runtime
  pure { activity, activityChanged, errors, errorRaised, runtime, state }

withHarness :: (Harness -> Aff Unit) -> Aff Unit
withHarness test = do
  harness <- makeHarness
  Aff.finally (liftEffect $ deactivate harness.runtime) (test harness)

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

awaitCounts :: Harness -> TaskCounts -> Aff Unit
awaitCounts harness expected = go 20
  where
  go remaining = do
    actual <- activityTotals <$> liftEffect (Ref.read harness.activity)
    if actual == expected then pure unit
    else if remaining <= 0 then
      fail $ "Expected activity " <> show expected <> " but got " <> show actual
    else do
      void $ await "activity update" harness.activityChanged
      go (remaining - 1)

contextName :: ErrorContext Unit Action -> String
contextName = case _ of
  ActivationError -> "activation"
  UpdateError _ -> "update"
  ActionError _ -> "action"
