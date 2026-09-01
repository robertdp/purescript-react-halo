module React.Halo.Internal.Task
  ( State
  , Status(..)
  , _Active
  , _Failed
  , _Idle
  , _Succeeded
  , asStatus
  , debounce
  , debounceWith
  , idle
  , isActive
  , once
  , reset
  , startIfInactive
  , supersede
  , toMaybe
  , toStatus
  ) where

import Prelude

import Control.Monad.State.Class (state)
import Data.Either (Either(..))
import Data.Lens (ALens', Getter', Prism', prism', to, withLens)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as Aff
import React.Halo.Internal.Runtime (HaloM, managedReset, managedStart)
import React.Halo.Internal.Types (ForkId)

-- | Read-only task lifecycle projected from abstract component state.
data Status error result
  = Idle
  | Active
  | Failed error
  | Succeeded result

derive instance eqStatus :: (Eq error, Eq result) => Eq (Status error result)

instance showStatus :: (Show error, Show result) => Show (Status error result) where
  show = case _ of
    Idle -> "Idle"
    Active -> "Active"
    Failed error -> "(Failed " <> show error <> ")"
    Succeeded result -> "(Succeeded " <> show result <> ")"

data Lifecycle error result
  = LifecycleIdle
  | LifecycleActive Run
  | LifecycleFailed error
  | LifecycleSucceeded result

type Run =
  { forkId :: ForkId
  , generation :: Int
  , sequence :: Int
  }

-- | Task lifecycle stored inside component state. The constructor is kept
-- | private by `React.Halo.Task` because active values carry runtime ownership.
newtype State error result = State
  { lifecycle :: Lifecycle error result
  , nextSequence :: Int
  }

-- | Initial task state.
idle :: forall error result. State error result
idle = State { lifecycle: LifecycleIdle, nextSequence: 0 }

-- | Project abstract task state to its public status.
toStatus :: forall error result. State error result -> Status error result
toStatus (State task) = case task.lifecycle of
  LifecycleIdle -> Idle
  LifecycleActive _ -> Active
  LifecycleFailed error -> Failed error
  LifecycleSucceeded result -> Succeeded result

-- | Read-only optic from task state to public status.
asStatus :: forall error result. Getter' (State error result) (Status error result)
asStatus = to toStatus

-- | Return a successful result, if present.
toMaybe :: forall error result. State error result -> Maybe result
toMaybe (State task) = case task.lifecycle of
  LifecycleSucceeded result -> Just result
  _ -> Nothing

-- | Test whether a debounce timer or task body is active.
isActive :: forall error result. State error result -> Boolean
isActive (State task) = case task.lifecycle of
  LifecycleActive _ -> true
  _ -> false

_Idle :: forall error result. Prism' (Status error result) Unit
_Idle = prism' (const Idle) case _ of
  Idle -> Just unit
  _ -> Nothing

_Active :: forall error result. Prism' (Status error result) Unit
_Active = prism' (const Active) case _ of
  Active -> Just unit
  _ -> Nothing

_Failed :: forall error result. Prism' (Status error result) error
_Failed = prism' Failed case _ of
  Failed error -> Just error
  _ -> Nothing

_Succeeded :: forall error result. Prism' (Status error result) result
_Succeeded = prism' Succeeded case _ of
  Succeeded result -> Just result
  _ -> Nothing

data Policy
  = Once
  | IfInactive
  | Supersede

-- | Start only from `Idle`. Typed failure and success remain terminal until
-- | `reset`.
once
  :: forall props componentState action m error result
   . ALens' componentState (State error result)
  -> HaloM props componentState action m (Either error result)
  -> HaloM props componentState action m Unit
once = launch Once Nothing

-- | Start from `Idle`, `Failed`, or `Succeeded`, but preserve active work.
startIfInactive
  :: forall props componentState action m error result
   . ALens' componentState (State error result)
  -> HaloM props componentState action m (Either error result)
  -> HaloM props componentState action m Unit
startIfInactive = launch IfInactive Nothing

-- | Make a new invocation authoritative immediately, fencing and requesting
-- | cancellation of prior managed work without waiting for its finalizers.
supersede
  :: forall props componentState action m error result
   . ALens' componentState (State error result)
  -> HaloM props componentState action m (Either error result)
  -> HaloM props componentState action m Unit
supersede = launch Supersede Nothing

-- | Trailing-edge latest-wins task invocation. The private cancellable timer and
-- | the executing body both project to `Active`.
debounce
  :: forall props componentState action m error result
   . ALens' componentState (State error result)
  -> Milliseconds
  -> HaloM props componentState action m (Either error result)
  -> HaloM props componentState action m Unit
debounce = debounceWith Aff.delay

-- Internal deterministic scheduler seam used by runtime tests.
debounceWith
  :: forall props componentState action m error result
   . (Milliseconds -> Aff Unit)
  -> ALens' componentState (State error result)
  -> Milliseconds
  -> HaloM props componentState action m (Either error result)
  -> HaloM props componentState action m Unit
debounceWith schedule target duration =
  launch Supersede (Just (schedule (nonNegative duration))) target

-- | Clear terminal state or cancel active work. Active cancellation is fenced
-- | synchronously and waits for Aff finalizers before returning.
reset
  :: forall props componentState action m error result
   . ALens' componentState (State error result)
  -> HaloM props componentState action m Unit
reset target = withLens target \getTask setTask ->
  managedReset \generation componentState ->
    let
      State task = getTask componentState
      nextState = State (task { lifecycle = LifecycleIdle })
      replacement = setTask componentState nextState
    in
      case task.lifecycle of
        LifecycleIdle -> Nothing
        LifecycleActive run -> Just
          { cancel: if run.generation == generation then Just run.forkId else Nothing
          , state: replacement
          }
        LifecycleFailed _ -> Just { cancel: Nothing, state: replacement }
        LifecycleSucceeded _ -> Just { cancel: Nothing, state: replacement }

launch
  :: forall props componentState action m error result
   . Policy
  -> Maybe (Aff Unit)
  -> ALens' componentState (State error result)
  -> HaloM props componentState action m (Either error result)
  -> HaloM props componentState action m Unit
launch policy privateDelay target body = withLens target \getTask setTask ->
  managedStart privateDelay \generation forkId componentState -> do
    claimed <- claim policy generation forkId (getTask componentState)
    pure
      { cancel: claimed.cancel
      , computation: complete getTask setTask claimed.run body
      , onExit: clear getTask setTask claimed.run
      , state: setTask componentState claimed.state
      }

claim
  :: forall error result
   . Policy
  -> Int
  -> ForkId
  -> State error result
  -> Maybe
       { cancel :: Maybe ForkId
       , run :: Run
       , state :: State error result
       }
claim policy generation forkId (State task) = do
  let
    currentRun = case task.lifecycle of
      LifecycleActive run | run.generation == generation -> Just run
      _ -> Nothing
    canStart = case policy of
      Once -> case task.lifecycle of
        LifecycleIdle -> true
        LifecycleActive run -> run.generation /= generation
        _ -> false
      IfInactive -> case currentRun of
        Just _ -> false
        Nothing -> true
      Supersede -> true
  if canStart then do
    let
      run =
        { forkId
        , generation
        , sequence: task.nextSequence
        }
      nextState = State
        { lifecycle: LifecycleActive run
        , nextSequence: task.nextSequence + 1
        }
    pure
      { cancel: case policy of
          Supersede -> _.forkId <$> currentRun
          _ -> Nothing
      , run
      , state: nextState
      }
  else Nothing

complete
  :: forall props componentState action m error result
   . (componentState -> State error result)
  -> (componentState -> State error result -> componentState)
  -> Run
  -> HaloM props componentState action m (Either error result)
  -> HaloM props componentState action m Unit
complete getTask setTask run body = do
  outcome <- body
  state \componentState ->
    let
      replacement = updateMatching run outcome (getTask componentState)
    in
      Tuple unit $ fromMaybe componentState (setTask componentState <$> replacement)

clear
  :: forall componentState error result
   . (componentState -> State error result)
  -> (componentState -> State error result -> componentState)
  -> Run
  -> componentState
  -> Maybe componentState
clear getTask setTask run componentState =
  setTask componentState <$> clearMatching run (getTask componentState)

updateMatching
  :: forall error result
   . Run
  -> Either error result
  -> State error result
  -> Maybe (State error result)
updateMatching run outcome (State task) = case task.lifecycle of
  LifecycleActive current | sameRun run current -> Just $ State
    ( task
        { lifecycle = case outcome of
            Left error -> LifecycleFailed error
            Right result -> LifecycleSucceeded result
        }
    )
  _ -> Nothing

clearMatching :: forall error result. Run -> State error result -> Maybe (State error result)
clearMatching run (State task) = case task.lifecycle of
  LifecycleActive current | sameRun run current ->
    Just $ State (task { lifecycle = LifecycleIdle })
  _ -> Nothing

sameRun :: Run -> Run -> Boolean
sameRun left right =
  left.generation == right.generation && left.sequence == right.sequence

nonNegative :: Milliseconds -> Milliseconds
nonNegative (Milliseconds duration) = Milliseconds (max 0.0 duration)
