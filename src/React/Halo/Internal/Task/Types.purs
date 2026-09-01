module React.Halo.Internal.Task.Types
  ( Binding
  , Slot
  , State
  , Status(..)
  , Token
  , View
  , _Active
  , _Failed
  , _Idle
  , _Succeeded
  , activateSlot
  , bindingBrand
  , bindingOf
  , clearBinding
  , completeSlot
  , emptyView
  , idle
  , idleSlot
  , isActive
  , makeToken
  , makeView
  , reconcileBinding
  , sameBindingFocus
  , sameToken
  , slot
  , slotBrand
  , statusAt
  , toMaybe
  , toStatus
  , tokenForkId
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Lens (ALens', Prism', prism', withLens)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import React.Halo.Internal.Types (ForkId, RuntimeId(..))
import Type.Proxy (Proxy)
import Unsafe.Reference (unsafeRefEq)

-- | Public task lifecycle. Runtime ownership remains hidden in `State` and
-- | `View`.
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

newtype Probe = Probe (Ref Unit)

newtype Token = Token
  { brand :: String
  , forkId :: ForkId
  , generation :: Int
  , runtimeId :: RuntimeId
  }

data Lifecycle error result
  = LifecycleIdle
  | LifecycleActive Token
  | LifecycleFailed error
  | LifecycleSucceeded result
  | LifecycleProbe Probe

-- | Freely copyable task lifecycle state. Active authority is validated against
-- | a runtime `View`; the value alone does not own a managed root.
newtype State error result = State
  { brand :: String
  , lifecycle :: Lifecycle error result
  }

-- | An identity-bearing optic for one task state focus. The name brands values
-- | while the lens locates the canonical component-state field.
data Slot (name :: Symbol) componentState error result = Slot
  { binding :: Binding componentState
  , brand :: String
  , get :: componentState -> State error result
  , set :: componentState -> State error result -> componentState
  }

type role Slot nominal representational representational representational

-- | Immutable task-authority snapshot published with component state.
newtype View state = View
  { authorities :: Map String Token
  , state :: state
  }

newtype Binding state = Binding
  { brand :: String
  , clear :: Token -> state -> Maybe state
  , mark :: Probe -> state -> state
  , reconcile ::
      Maybe Token
      -> state
      -> { authority :: Maybe Token
         , changed :: Boolean
         , displaced :: Maybe Token
         , state :: state
         }
  , sees :: Probe -> state -> Boolean
  }

-- | Construct a branded task slot from a type-level name and lawful lens.
-- |
-- | On first policy use, a runtime binds the brand to the lens focus. Reusing a
-- | brand at another focus or another brand at the same focus fails in the
-- | calling root's existing error context before mutation or cancellation.
slot
  :: forall name componentState error result
   . IsSymbol name
  => Proxy name
  -> ALens' componentState (State error result)
  -> Slot name componentState error result
slot proxy target = withLens target \get set ->
  let
    brand = reflectSymbol proxy
    binding = Binding
      { brand
      , clear: \token componentState -> case get componentState of
          State task -> case task.lifecycle of
            LifecycleActive current
              | task.brand == brand && sameToken token current ->
                  Just $ set componentState (State { brand, lifecycle: LifecycleIdle })
            _ -> Nothing
      , mark: \probe componentState ->
          set componentState (State { brand, lifecycle: LifecycleProbe probe })
      , reconcile: reconcileFocus brand get set
      , sees: \probe componentState -> case get componentState of
          State task -> case task.lifecycle of
            LifecycleProbe candidate -> sameProbe probe candidate
            _ -> false
      }
  in
    Slot { binding, brand, get, set }

-- | Construct correctly branded idle state for a slot.
idle :: forall name state error result. Slot name state error result -> State error result
idle (Slot target) = State { brand: target.brand, lifecycle: LifecycleIdle }

-- | Project one slot through an immutable runtime view.
toStatus
  :: forall name state error result
   . View state
  -> Slot name state error result
  -> Status error result
toStatus (View snapshot) target@(Slot slotState) =
  statusAt target (Map.lookup slotState.brand snapshot.authorities) snapshot.state

-- | Return a slot's authoritative successful result, if present.
toMaybe
  :: forall name state error result
   . View state
  -> Slot name state error result
  -> Maybe result
toMaybe taskView target = case toStatus taskView target of
  Succeeded result -> Just result
  _ -> Nothing

-- | Test whether a slot has an authoritative debounce timer or task body.
isActive
  :: forall name state error result
   . View state
  -> Slot name state error result
  -> Boolean
isActive taskView target = case toStatus taskView target of
  Active -> true
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

bindingOf :: forall name state error result. Slot name state error result -> Binding state
bindingOf (Slot target) = target.binding

bindingBrand :: forall state. Binding state -> String
bindingBrand (Binding binding) = binding.brand

clearBinding :: forall state. Binding state -> Token -> state -> Maybe state
clearBinding (Binding binding) = binding.clear

slotBrand :: forall name state error result. Slot name state error result -> String
slotBrand (Slot target) = target.brand

makeToken
  :: forall name state error result
   . Slot name state error result
  -> RuntimeId
  -> Int
  -> ForkId
  -> Token
makeToken (Slot target) runtimeId generation forkId = Token
  { brand: target.brand
  , forkId
  , generation
  , runtimeId
  }

tokenForkId :: Token -> ForkId
tokenForkId (Token token) = token.forkId

sameToken :: Token -> Token -> Boolean
sameToken (Token left) (Token right) =
  left.brand == right.brand
    && left.generation == right.generation
    && left.forkId == right.forkId
    && sameRuntime left.runtimeId right.runtimeId

statusAt
  :: forall name state error result
   . Slot name state error result
  -> Maybe Token
  -> state
  -> Status error result
statusAt (Slot target) authority componentState = case target.get componentState of
  State task
    | task.brand /= target.brand -> Idle
    | otherwise -> case task.lifecycle of
        LifecycleIdle -> Idle
        LifecycleActive token -> case authority of
          Just current | sameToken token current -> Active
          _ -> Idle
        LifecycleFailed error -> Failed error
        LifecycleSucceeded result -> Succeeded result
        LifecycleProbe _ -> Idle

activateSlot
  :: forall name state error result
   . Slot name state error result
  -> Token
  -> state
  -> state
activateSlot (Slot target) token componentState =
  target.set componentState (State { brand: target.brand, lifecycle: LifecycleActive token })

idleSlot
  :: forall name state error result
   . Slot name state error result
  -> state
  -> state
idleSlot target@(Slot current) componentState = current.set componentState (idle target)

completeSlot
  :: forall name state error result
   . Slot name state error result
  -> Token
  -> Either error result
  -> state
  -> Maybe state
completeSlot (Slot target) token outcome componentState = case target.get componentState of
  State task -> case task.lifecycle of
    LifecycleActive current
      | task.brand == target.brand && sameToken token current ->
          Just $ target.set componentState $ State
            { brand: target.brand
            , lifecycle: case outcome of
                Left error -> LifecycleFailed error
                Right result -> LifecycleSucceeded result
            }
    _ -> Nothing

emptyView :: forall state. state -> View state
emptyView state = View { authorities: Map.empty, state }

makeView :: forall state. state -> Map String Token -> View state
makeView state authorities = View { authorities, state }

sameBindingFocus :: forall state. state -> Binding state -> Binding state -> Effect Boolean
sameBindingFocus componentState (Binding left) (Binding right) = do
  probeRef <- Ref.new unit
  let probe = Probe probeRef
  pure $
    left.sees probe (right.mark probe componentState)
      && right.sees probe (left.mark probe componentState)

reconcileBinding
  :: forall state
   . Binding state
  -> Maybe Token
  -> state
  -> { authority :: Maybe Token
     , changed :: Boolean
     , displaced :: Maybe Token
     , state :: state
     }
reconcileBinding (Binding binding) = binding.reconcile

reconcileFocus
  :: forall state error result
   . String
  -> (state -> State error result)
  -> (state -> State error result -> state)
  -> Maybe Token
  -> state
  -> { authority :: Maybe Token
     , changed :: Boolean
     , displaced :: Maybe Token
     , state :: state
     }
reconcileFocus brand get set authority componentState =
  let
    State task = get componentState
    correctBrand = task.brand == brand
    exactActive = case task.lifecycle, authority of
      LifecycleActive token, Just current -> correctBrand && sameToken token current
      _, _ -> false
    validInactive = correctBrand && case task.lifecycle of
      LifecycleIdle -> true
      LifecycleFailed _ -> true
      LifecycleSucceeded _ -> true
      _ -> false
  in
    if exactActive then
      { authority
      , changed: false
      , displaced: Nothing
      , state: componentState
      }
    else if validInactive then
      { authority: Nothing
      , changed: false
      , displaced: authority
      , state: componentState
      }
    else
      { authority: Nothing
      , changed: true
      , displaced: authority
      , state: set componentState (State { brand, lifecycle: LifecycleIdle })
      }

sameProbe :: Probe -> Probe -> Boolean
sameProbe (Probe left) (Probe right) = unsafeRefEq left right

sameRuntime :: RuntimeId -> RuntimeId -> Boolean
sameRuntime (RuntimeId left) (RuntimeId right) = unsafeRefEq left right
