module Test.Halo.Helpers
  ( Action(..)
  , Gate
  , Harness
  , Key(..)
  , UnitTask
  , WorkInput
  , WorkTask
  , await
  , awaitCounts
  , handlers
  , makeGate
  , makeHarness
  , release
  , runGate
  , shouldNotHaveStarted
  , withHarness
  , work
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
import Effect.AVar (AVar)
import Effect.AVar as EffectAVar
import Effect.Class (liftEffect)
import Effect.Exception (message)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import React.Halo as Halo
import React.Halo.Handlers (Handlers, defaultHandlers)
import React.Halo.Internal.Runtime (HaloM, Runtime, activate, createRuntime, deactivate, fork)
import React.Halo.Internal.Types (Activity, ErrorContext(..), TaskCounts, emptyActivity, totalActivity)
import Test.Spec.Assertions (fail, shouldEqual)

data Key = Search | Save

derive instance eqKey :: Eq Key
derive instance ordKey :: Ord Key

instance showKey :: Show Key where
  show Search = "Search"
  show Save = "Save"

type Gate =
  { launched :: AVar Unit
  , release :: AVar Unit
  , settled :: AVar Unit
  , started :: AVar Unit
  }

type WorkInput =
  { gate :: Gate
  , value :: Int
  }

type WorkTask = Halo.Task Unit (Array Int) Action Key WorkInput

type UnitTask = Halo.Task Unit (Array Int) Action Key Unit

data Action
  = Perform WorkTask WorkInput
  | PerformWithWitness WorkTask WorkInput Gate
  | PerformUnit UnitTask Gate
  | Cancel WorkTask (AVar Unit)
  | Direct Int Gate
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
  launched <- EffectAVar.empty
  started <- EffectAVar.empty
  releaseGate <- EffectAVar.empty
  settled <- EffectAVar.empty
  pure { launched, started, release: releaseGate, settled }

runGate
  :: forall props action key
   . Int
  -> Gate
  -> HaloM props (Array Int) action key Unit
runGate value gate = do
  liftAff $ Aff.finally
    (void $ AVar.tryPut unit gate.settled)
    do
      AVar.put unit gate.started
      void $ AVar.take gate.release
  modify_ (flip Array.snoc value)

work :: WorkInput -> HaloM Unit (Array Int) Action Key Unit
work input = runGate input.value input.gate

handlers :: Handlers Unit (Array Int) Action Key
handlers = defaultHandlers
  { onAction = case _ of
      Perform task input -> do
        Halo.perform task input
        liftAff $ void $ AVar.tryPut unit input.gate.launched
      PerformWithWitness task input witness -> do
        Halo.perform task input
        void $ fork (runGate 999 witness)
        liftAff $ void $ AVar.take witness.started
        liftAff $ void $ AVar.tryPut unit input.gate.launched
      PerformUnit task gate -> do
        Halo.perform_ task
        liftAff $ void $ AVar.tryPut unit gate.launched
      Cancel task completed -> do
        Halo.cancel task
        liftAff $ void $ AVar.tryPut unit completed
      Direct value gate -> do
        liftAff $ void $ AVar.tryPut unit gate.launched
        runGate value gate
      Boom gate -> do
        liftAff $ void $ AVar.tryPut unit gate.launched
        liftAff $ Aff.finally
          (void $ AVar.tryPut unit gate.settled)
          (Aff.throwError (Aff.error "boom"))
  }

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
        { handlers
        , onError: \context error -> do
            Ref.modify_ (\current -> Array.snoc current (contextName context <> ": " <> message error)) errors
            void $ EffectAVar.tryPut unit errorRaised
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
    actual <- totalActivity <$> liftEffect (Ref.read harness.activity)
    if actual == expected then pure unit
    else if remaining <= 0 then
      fail $ "Expected activity " <> show expected <> " but got " <> show actual
    else do
      void $ await "activity update" harness.activityChanged
      go (remaining - 1)

contextName :: ErrorContext Unit Action Key -> String
contextName = case _ of
  ActivationError -> "activation"
  DeactivationError -> "deactivation"
  PropsChangeError _ -> "props"
  ActionError _ -> "action"
  TaskError key -> "task " <> show key
  TaskConfigurationError key -> "task configuration " <> show key
