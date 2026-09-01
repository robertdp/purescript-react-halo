module React.Halo.Internal.Runtime
  ( HaloAp
  , HaloM
  , Handlers
  , Runtime
  , activate
  , createRuntime
  , deactivate
  , dispatch
  , fork
  , getProps
  , kill
  , managedReset
  , managedStart
  , registerCleanup
  , releaseCleanup
  , subscribe
  , subscribeWithId
  , syncSpec
  , unsubscribe
  , updateProps
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Reader (ReaderT(..), class MonadAsk, ask, mapReaderT)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer (class MonadTell, tell)
import Control.Parallel (class Parallel, parallel, sequential)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Error, Fiber, ParAff)
import Effect.Aff as Aff
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.AVar as EffectAVar
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception as Exception
import Effect.Ref (Ref)
import Effect.Ref as Ref
import React.Halo.Internal.Types (CleanupId(..), ErrorContext(..), ForkId(..), SubscriptionId(..))
import React.Halo.Subscription (Emitter)
import React.Halo.Subscription as Subscription
import Unsafe.Reference (unsafeRefEq)

-- | A sequential component computation over an application monad `m`.
-- |
-- | Standard `lift` runs an `m` value through the interpreter supplied to
-- | `component` or `useHalo`. Every lift checks the current ownership fence, so
-- | killed or deactivated work cannot begin another application effect. HaloM
-- | also provides component `MonadState`; its other lifted capabilities, such
-- | as `MonadEffect`, `MonadAff`, `MonadAsk`, `MonadTell`, and `MonadThrow`, pass
-- | through `m`.
newtype HaloM props state action (m :: Type -> Type) a = HaloM
  (ReaderT (Execution props state action m) Aff a)

-- | The abstract parallel applicative counterpart of `HaloM`.
-- |
-- | Parallel branches share one root, component scope, and application
-- | interpreter snapshot. Concurrent component-state writes have
-- | nondeterministic ordering; prefer combining independent application results
-- | before one state update.
newtype HaloAp props state action (m :: Type -> Type) a = HaloAp
  (ReaderT (Execution props state action m) ParAff a)

derive newtype instance functorHaloM :: Functor (HaloM props state action m)
derive newtype instance applyHaloM :: Apply (HaloM props state action m)
derive newtype instance applicativeHaloM :: Applicative (HaloM props state action m)
derive newtype instance bindHaloM :: Bind (HaloM props state action m)
derive newtype instance monadHaloM :: Monad (HaloM props state action m)

derive newtype instance functorHaloAp :: Functor (HaloAp props state action m)
derive newtype instance applyHaloAp :: Apply (HaloAp props state action m)
derive newtype instance applicativeHaloAp :: Applicative (HaloAp props state action m)

instance monadTransHaloM :: MonadTrans (HaloM props state action) where
  lift value = HaloM $ ReaderT \execution -> do
    current <- liftEffect $ isCurrent execution
    if current then
      case execution.runInAff of
        RunInAff run -> run value
    else Aff.throwError scopeCancellationError

-- Public effect capabilities deliberately pass through the application monad.
instance monadEffectHaloM :: MonadEffect m => MonadEffect (HaloM props state action m) where
  liftEffect = lift <<< liftEffect

instance monadAffHaloM :: MonadAff m => MonadAff (HaloM props state action m) where
  liftAff = lift <<< liftAff

instance monadAskHaloM :: MonadAsk r m => MonadAsk r (HaloM props state action m) where
  ask = lift ask

instance monadTellHaloM :: MonadTell w m => MonadTell w (HaloM props state action m) where
  tell = lift <<< tell

instance monadThrowHaloM :: MonadThrow error m => MonadThrow error (HaloM props state action m) where
  throwError = lift <<< throwError

instance parallelHaloM :: Parallel (HaloAp props state action m) (HaloM props state action m) where
  parallel (HaloM computation) = HaloAp (mapReaderT parallel computation)
  sequential (HaloAp computation) = HaloM (mapReaderT sequential computation)

instance monadStateHaloM :: MonadState state (HaloM props state action m) where
  state updateState = HaloM $ ReaderT \execution ->
    liftEffect do
      current <- isCurrent execution
      let Runtime runtime = execution.runtime
      oldState <- Ref.read runtime.state
      let Tuple result newState = updateState oldState
      if current then do
        unless (unsafeRefEq oldState newState) do
          Ref.write newState runtime.state
          update <- Ref.read runtime.stateUpdate
          update newState
        pure result
      else pure result

-- | Activation, prop-change, and action callbacks.
-- |
-- | Every invocation starts an independent root in the current React
-- | activation. Handlers can overlap and are cancelled when that activation
-- | deactivates. `onPropsChange` receives the previous props; `getProps` reads
-- | the current props.
type Handlers props state action m =
  { onActivate :: HaloM props state action m Unit
  , onPropsChange :: props -> HaloM props state action m Unit
  , onAction :: action -> HaloM props state action m Unit
  }

type RuntimeSpec props state action m =
  { handlers :: Handlers props state action m
  , onError :: ErrorContext props action -> Error -> Effect Unit
  }

newtype RunInAff m = RunInAff (m ~> Aff)

newtype Runtime props state action m = Runtime
  { fresh :: Ref Int
  , needsStatePublish :: Ref Boolean
  , props :: Ref props
  , runInAff :: Ref (RunInAff m)
  , scope :: Ref (Maybe Scope)
  , spec :: Ref (RuntimeSpec props state action m)
  , state :: Ref state
  , stateUpdate :: Ref (state -> Effect Unit)
  }

newtype Scope = Scope
  { active :: Ref Boolean
  , cleanups :: Ref (Map CleanupId (Effect Unit))
  , forks :: Ref (Map ForkId Root)
  , generation :: Int
  , handlers :: Ref (Map Int Root)
  , subscriptions :: Ref (Map SubscriptionId (Effect Unit))
  }

newtype Owner = Owner
  { alive :: Ref Boolean }

newtype Root = Root
  { fiber :: Fiber Unit
  , onDeactivate :: Effect Unit
  , owner :: Owner
  }

type Execution props state action m =
  { context :: ErrorContext props action
  , owner :: Owner
  , runInAff :: RunInAff m
  , runtime :: Runtime props state action m
  , scope :: Scope
  }

type Prepared =
  { root :: Root
  , start :: Effect Unit
  }

createRuntime
  :: forall props state action m
   . (m ~> Aff)
  -> { initialProps :: props
     , initialState :: state
     , spec :: RuntimeSpec props state action m
     , stateUpdate :: state -> Effect Unit
     }
  -> Effect (Runtime props state action m)
createRuntime runInAff input = do
  freshRef <- Ref.new 0
  needsStatePublish <- Ref.new false
  propsRef <- Ref.new input.initialProps
  runInAffRef <- Ref.new (RunInAff runInAff)
  scope <- Ref.new Nothing
  spec <- Ref.new input.spec
  state <- Ref.new input.initialState
  stateUpdate <- Ref.new input.stateUpdate
  pure $ Runtime
    { fresh: freshRef
    , needsStatePublish
    , props: propsRef
    , runInAff: runInAffRef
    , scope
    , spec
    , state
    , stateUpdate
    }

-- | Update render-owned callbacks and the interpreter used by roots started
-- | after this synchronization. Running roots retain their interpreter snapshot.
syncSpec
  :: forall props state action m
   . Runtime props state action m
  -> (m ~> Aff)
  -> { spec :: RuntimeSpec props state action m
     , stateUpdate :: state -> Effect Unit
     }
  -> Effect Unit
syncSpec (Runtime runtime) runInAff input = do
  Ref.write (RunInAff runInAff) runtime.runInAff
  Ref.write input.spec runtime.spec
  Ref.write input.stateUpdate runtime.stateUpdate

activate :: forall props state action m. Runtime props state action m -> Effect Unit
activate runtime@(Runtime state) = do
  activeScope <- Ref.read state.scope
  case activeScope of
    Just _ -> pure unit
    Nothing -> do
      generation <- fresh runtime
      active <- Ref.new true
      cleanups <- Ref.new Map.empty
      forks <- Ref.new Map.empty
      handlers <- Ref.new Map.empty
      subscriptions <- Ref.new Map.empty
      let scope = Scope { active, cleanups, forks, generation, handlers, subscriptions }
      Ref.write (Just scope) state.scope
      publishRuntimeState runtime
      spec <- Ref.read state.spec
      startHandler runtime scope ActivationError spec.handlers.onActivate

deactivate :: forall props state action m. Runtime props state action m -> Effect Unit
deactivate (Runtime state) = do
  activeScope <- Ref.read state.scope
  case activeScope of
    Nothing -> pure unit
    Just (Scope current) -> do
      Ref.write false current.active
      Ref.write Nothing state.scope

      handlers <- takeRef current.handlers Map.empty
      forks <- takeRef current.forks Map.empty
      cleanups <- takeRef current.cleanups Map.empty
      subscriptions <- takeRef current.subscriptions Map.empty
      let roots = Map.values handlers <> Map.values forks

      -- Fence every root before normalizing managed state, invoking foreign
      -- cleanup, or requesting cooperative Aff cancellation.
      traverse_ fenceRoot roots
      traverse_ runRootDeactivation roots
      cleanupResults <- traverse Exception.try
        (Map.values cleanups <> Map.values subscriptions)
      traverse_ requestCancel roots

      -- A faulty external cleanup must not prevent any other cleanup request.
      spec <- Ref.read state.spec
      traverse_
        ( case _ of
            Left error -> spec.onError DeactivationError error
            Right _ -> pure unit
        )
        cleanupResults

updateProps
  :: forall props state action m
   . Runtime props state action m
  -> props
  -> Effect Unit
updateProps runtime@(Runtime state) newProps = do
  previousProps <- Ref.read state.props
  unless (unsafeRefEq previousProps newProps) do
    Ref.write newProps state.props
    activeScope <- Ref.read state.scope
    traverse_
      ( \scope -> do
          spec <- Ref.read state.spec
          startHandler runtime scope (PropsChangeError previousProps) (spec.handlers.onPropsChange previousProps)
      )
      activeScope

dispatch
  :: forall props state action m
   . Runtime props state action m
  -> action
  -> Effect Unit
dispatch runtime@(Runtime state) action = do
  activeScope <- Ref.read state.scope
  traverse_ (\scope -> dispatchToScope runtime scope action) activeScope

dispatchToScope
  :: forall props state action m
   . Runtime props state action m
  -> Scope
  -> action
  -> Effect Unit
dispatchToScope runtime@(Runtime state) scope action = do
  current <- isScopeCurrent runtime scope
  when current do
    spec <- Ref.read state.spec
    startHandler runtime scope (ActionError action) (spec.handlers.onAction action)

-- | Read the latest component props, rather than the props captured when the
-- | current root started.
getProps :: forall props state action m. HaloM props state action m props
getProps = HaloM $ ReaderT \execution -> do
  let Runtime runtime = execution.runtime
  liftEffect $ Ref.read runtime.props

-- | Start a process owned by the current React activation.
-- |
-- | The fork may outlive its launching handler and has an independent
-- | cancellation fence, but it inherits that root's application-interpreter
-- | snapshot. It is cancelled by `kill` or activation deactivation. Unexpected
-- | failure is reported as `ForkError`.
fork
  :: forall props state action m
   . HaloM props state action m Unit
  -> HaloM props state action m ForkId
fork child = HaloM $ ReaderT \execution -> do
  fid <- liftEffect $ ForkId <$> fresh execution.runtime
  current <- liftEffect $ isCurrent execution
  when current do
    prepared <- liftEffect $ prepare execution.runInAff execution.runtime execution.scope (ForkError fid) child
      { onComplete: do
          let Scope scope = execution.scope
          Ref.modify_ (Map.delete fid) scope.forks
      , onDeactivate: pure unit
      , onUnexpected: pure unit
      }
    liftEffect do
      let Scope scope = execution.scope
      Ref.modify_ (Map.insert fid prepared.root) scope.forks
      prepared.start
  pure fid

-- | Cancel a component-owned fork.
-- |
-- | Halo removes and fences the fork synchronously, then waits for Aff
-- | cancellation and finalizers. The fence blocks later state commits, Halo
-- | capabilities, and lifted application effects. An unknown or completed ID
-- | is ignored.
kill
  :: forall props state action m
   . ForkId
  -> HaloM props state action m Unit
kill fid = HaloM $ ReaderT \execution -> do
  current <- liftEffect $ isCurrent execution
  when current do
    let Scope scope = execution.scope
    root <- liftEffect $ Ref.modify'
      ( \forks ->
          { state: Map.delete fid forks
          , value: Map.lookup fid forks
          }
      )
      scope.forks
    traverse_
      ( \forkRoot -> do
          liftEffect $ fenceRoot forkRoot
          cancelRootAff forkRoot
      )
      root

-- Internal managed roots support state-focused lifecycle APIs without exposing
-- root identity. Claiming state, fencing prior work, registering the new root,
-- and opening its start gate happen in one synchronous runtime transaction.
managedStart
  :: forall props state action m
   . Maybe (Aff Unit)
  -> ( Int
       -> ForkId
       -> state
       -> Maybe
            { cancel :: Maybe ForkId
            , computation :: HaloM props state action m Unit
            , onExit :: state -> Maybe state
            , state :: state
            }
     )
  -> HaloM props state action m Unit
managedStart privateDelay claim = HaloM $ ReaderT \execution ->
  liftEffect do
    current <- isCurrent execution
    when current do
      fid <- ForkId <$> fresh execution.runtime
      let Scope scope = execution.scope
      oldState <- readRuntimeState execution.runtime
      case claim scope.generation fid oldState of
        Nothing -> pure unit
        Just managed -> do
          let
            applyExit publish = applyManagedState publish execution.runtime managed.onExit
          prepared <- prepare execution.runInAff execution.runtime execution.scope (ForkError fid)
            (withPrivateDelay privateDelay managed.computation)
            { onComplete: Ref.modify_ (Map.delete fid) scope.forks
            , onDeactivate: applyExit false
            , onUnexpected: applyExit true
            }
          previous <- Ref.modify'
            ( \forks ->
                { state: case managed.cancel of
                    Nothing -> forks
                    Just cancelId -> Map.delete cancelId forks
                , value: managed.cancel >>= flip Map.lookup forks
                }
            )
            scope.forks
          traverse_ fenceRoot previous
          writeRuntimeState execution.runtime managed.state
          Ref.modify_ (Map.insert fid prepared.root) scope.forks
          update <- readStateUpdate execution.runtime
          update managed.state
          traverse_ requestCancel previous
          prepared.start

-- Stop managed work after atomically publishing its replacement state. A root
-- that resets itself is cancelled by unwinding its own fiber rather than trying
-- to join itself.
managedReset
  :: forall props state action m
   . ( Int
       -> state
       -> Maybe
            { cancel :: Maybe ForkId
            , state :: state
            }
     )
  -> HaloM props state action m Unit
managedReset transition = HaloM $ ReaderT \execution -> do
  current <- liftEffect $ isCurrent execution
  when current do
    root <- liftEffect do
      let Scope scope = execution.scope
      oldState <- readRuntimeState execution.runtime
      case transition scope.generation oldState of
        Nothing -> pure Nothing
        Just next -> do
          previous <- Ref.modify'
            ( \forks ->
                { state: case next.cancel of
                    Nothing -> forks
                    Just cancelId -> Map.delete cancelId forks
                , value: next.cancel >>= flip Map.lookup forks
                }
            )
            scope.forks
          traverse_ fenceRoot previous
          writeRuntimeState execution.runtime next.state
          update <- readStateUpdate execution.runtime
          update next.state
          pure previous
    traverse_
      ( \managedRoot ->
          if sameOwner execution.owner managedRoot then
            Aff.throwError scopeCancellationError
          else cancelRootAff managedRoot
      )
      root

-- | Register synchronous `Effect` cleanup in the current activation scope.
-- |
-- | Deactivation attempts every remaining cleanup after fencing scope roots.
-- | Cleanup failures are isolated and reported as `DeactivationError` through
-- | the latest error callback.
registerCleanup
  :: forall props state action m
   . Effect Unit
  -> HaloM props state action m CleanupId
registerCleanup cleanup = HaloM $ ReaderT \execution -> do
  cid <- liftEffect $ CleanupId <$> fresh execution.runtime
  current <- liftEffect $ isCurrent execution
  when current do
    liftEffect do
      let Scope scope = execution.scope
      Ref.modify_ (Map.insert cid cleanup) scope.cleanups
  pure cid

-- | Remove tracked cleanup before running it. Unknown or already released IDs
-- | are ignored. A throwing cleanup cannot be retried during deactivation and
-- | follows the error context of the handler or fork that releases it.
releaseCleanup
  :: forall props state action m
   . CleanupId
  -> HaloM props state action m Unit
releaseCleanup cid = HaloM $ ReaderT \execution -> do
  current <- liftEffect $ isCurrent execution
  when current do
    let Scope scope = execution.scope
    cleanup <- liftEffect $ Ref.modify'
      ( \cleanups ->
          { state: Map.delete cid cleanups
          , value: Map.lookup cid cleanups
          }
      )
      scope.cleanups
    liftEffect $ traverse_ identity cleanup

-- | Register an action emitter in the current activation scope.
-- |
-- | Its synchronous cleanup runs on manual unsubscription or deactivation.
-- | Emissions stay bound to the activation that registered them, so a retained
-- | stale callback cannot dispatch into a later activation.
subscribe
  :: forall props state action m
   . Emitter action
  -> HaloM props state action m SubscriptionId
subscribe = subscribeWithId <<< const

-- | Subscribe while providing the allocated identifier to the emitter's
-- | registration logic.
subscribeWithId
  :: forall props state action m
   . (SubscriptionId -> Emitter action)
  -> HaloM props state action m SubscriptionId
subscribeWithId makeEmitter = HaloM $ ReaderT \execution -> do
  sid <- liftEffect $ SubscriptionId <$> fresh execution.runtime
  current <- liftEffect $ isCurrent execution
  when current do
    cleanup <- liftEffect $ Subscription.runEmitter (makeEmitter sid)
      (dispatchToScope execution.runtime execution.scope)
    liftEffect do
      let Scope scope = execution.scope
      Ref.modify_ (Map.insert sid cleanup) scope.subscriptions
  pure sid

-- | Remove a tracked subscription before running its cleanup.
-- |
-- | A throwing cleanup therefore cannot be retried during deactivation; when
-- | called from a root, the failure is routed through that root's error context.
unsubscribe
  :: forall props state action m
   . SubscriptionId
  -> HaloM props state action m Unit
unsubscribe sid = HaloM $ ReaderT \execution -> do
  current <- liftEffect $ isCurrent execution
  when current do
    let Scope scope = execution.scope
    subscription <- liftEffect $ Ref.modify'
      ( \subscriptions ->
          { state: Map.delete sid subscriptions
          , value: Map.lookup sid subscriptions
          }
      )
      scope.subscriptions
    liftEffect $ traverse_ identity subscription

startHandler
  :: forall props state action m
   . Runtime props state action m
  -> Scope
  -> ErrorContext props action
  -> HaloM props state action m Unit
  -> Effect Unit
startHandler runtime@(Runtime state) scope@(Scope current) context computation = do
  runId <- fresh runtime
  runInAff <- Ref.read state.runInAff
  prepared <- prepare runInAff runtime scope context computation
    { onComplete: Ref.modify_ (Map.delete runId) current.handlers
    , onDeactivate: pure unit
    , onUnexpected: pure unit
    }
  Ref.modify_ (Map.insert runId prepared.root) current.handlers
  prepared.start

prepare
  :: forall props state action m
   . RunInAff m
  -> Runtime props state action m
  -> Scope
  -> ErrorContext props action
  -> HaloM props state action m Unit
  -> { onComplete :: Effect Unit
     , onDeactivate :: Effect Unit
     , onUnexpected :: Effect Unit
     }
  -> Effect Prepared
prepare runInAff runtime scope context computation hooks = do
  owner <- createOwner
  gate <- EffectAVar.empty
  fiber <- Aff.launchAff do
    void $ AVar.take gate
    Aff.finally
      ( liftEffect do
          let Owner current = owner
          Ref.write false current.alive
          hooks.onComplete
      )
      do
        outcome <- Aff.attempt $ Aff.supervise $
          runHaloM { context, owner, runInAff, runtime, scope } computation
        case outcome of
          Left error -> do
            current <- liftEffect $ isCurrent { context, owner, runInAff, runtime, scope }
            when current do
              liftEffect hooks.onUnexpected
              let Runtime state = runtime
              spec <- liftEffect $ Ref.read state.spec
              liftEffect $ spec.onError context error
          Right _ -> pure unit
  pure
    { root: Root { fiber, onDeactivate: hooks.onDeactivate, owner }
    , start: Aff.launchAff_ (AVar.put unit gate)
    }

runHaloM
  :: forall props state action m a
   . Execution props state action m
  -> HaloM props state action m a
  -> Aff a
runHaloM execution (HaloM computation) = case computation of
  ReaderT run -> run execution

withPrivateDelay
  :: forall props state action m a
   . Maybe (Aff Unit)
  -> HaloM props state action m a
  -> HaloM props state action m a
withPrivateDelay privateDelay computation = HaloM $ ReaderT \execution -> do
  traverse_ identity privateDelay
  runHaloM execution computation

applyManagedState
  :: forall props state action m
   . Boolean
  -> Runtime props state action m
  -> (state -> Maybe state)
  -> Effect Unit
applyManagedState publish (Runtime current) transition = do
  oldState <- Ref.read current.state
  case transition oldState of
    Nothing -> pure unit
    Just newState -> do
      Ref.write newState current.state
      if publish then do
        update <- Ref.read current.stateUpdate
        update newState
      else Ref.write true current.needsStatePublish

publishRuntimeState :: forall props state action m. Runtime props state action m -> Effect Unit
publishRuntimeState (Runtime runtime) = do
  needsPublish <- Ref.read runtime.needsStatePublish
  when needsPublish do
    Ref.write false runtime.needsStatePublish
    currentState <- Ref.read runtime.state
    update <- Ref.read runtime.stateUpdate
    update currentState

readRuntimeState :: forall props state action m. Runtime props state action m -> Effect state
readRuntimeState (Runtime runtime) = Ref.read runtime.state

writeRuntimeState :: forall props state action m. Runtime props state action m -> state -> Effect Unit
writeRuntimeState (Runtime runtime) = flip Ref.write runtime.state

readStateUpdate
  :: forall props state action m
   . Runtime props state action m
  -> Effect (state -> Effect Unit)
readStateUpdate (Runtime runtime) = Ref.read runtime.stateUpdate

createOwner :: Effect Owner
createOwner = do
  alive <- Ref.new true
  pure $ Owner { alive }

fenceRoot :: Root -> Effect Unit
fenceRoot (Root root) = do
  let Owner owner = root.owner
  Ref.write false owner.alive

runRootDeactivation :: Root -> Effect Unit
runRootDeactivation (Root root) = root.onDeactivate

sameOwner :: Owner -> Root -> Boolean
sameOwner (Owner left) (Root right) = case right.owner of
  Owner owner -> unsafeRefEq left.alive owner.alive

requestCancel :: Root -> Effect Unit
requestCancel root = Aff.launchAff_ (cancelRootAff root)

cancelRootAff :: Root -> Aff Unit
cancelRootAff root@(Root current) = do
  liftEffect $ fenceRoot root
  Aff.killFiber scopeCancellationError current.fiber

scopeCancellationError :: Error
scopeCancellationError = Aff.error "Halo scope cancelled"

isCurrent
  :: forall props state action m
   . Execution props state action m
  -> Effect Boolean
isCurrent execution = do
  let Owner owner = execution.owner
  ownerAlive <- Ref.read owner.alive
  scopeCurrent <- isScopeCurrent execution.runtime execution.scope
  pure (ownerAlive && scopeCurrent)

isScopeCurrent
  :: forall props state action m
   . Runtime props state action m
  -> Scope
  -> Effect Boolean
isScopeCurrent (Runtime runtime) (Scope scope) = do
  scopeActive <- Ref.read scope.active
  activeScope <- Ref.read runtime.scope
  pure $ scopeActive && case activeScope of
    Just (Scope active) -> active.generation == scope.generation
    Nothing -> false

fresh :: forall props state action m. Runtime props state action m -> Effect Int
fresh (Runtime runtime) = Ref.modify' (\value -> { state: value + 1, value }) runtime.fresh

takeRef :: forall a. Ref a -> a -> Effect a
takeRef ref replacement = Ref.modify' (\value -> { state: replacement, value }) ref
