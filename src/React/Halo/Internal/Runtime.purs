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
  , managedComplete
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
import Data.Foldable (foldM, foldl, traverse_)
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
import React.Halo.Internal.Task.Types (Binding, Token, View)
import React.Halo.Internal.Task.Types as Task
import React.Halo.Internal.Types (CleanupId(..), ErrorContext(..), ForkId(..), RuntimeId(..), SubscriptionId(..))
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
          commitReconciledState execution newState
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
  { bindings :: Ref (Map String (Binding state))
  , fresh :: Ref Int
  , needsStatePublish :: Ref Boolean
  , props :: Ref props
  , runInAff :: Ref (RunInAff m)
  , runtimeId :: RuntimeId
  , scope :: Ref (Maybe Scope)
  , spec :: Ref (RuntimeSpec props state action m)
  , state :: Ref state
  , stateUpdate :: Ref (state -> View state -> Effect Unit)
  }

newtype Scope = Scope
  { active :: Ref Boolean
  , authorities :: Ref (Map String Token)
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
     , stateUpdate :: state -> View state -> Effect Unit
     }
  -> Effect (Runtime props state action m)
createRuntime runInAff input = do
  bindings <- Ref.new Map.empty
  freshRef <- Ref.new 0
  needsStatePublish <- Ref.new false
  propsRef <- Ref.new input.initialProps
  runInAffRef <- Ref.new (RunInAff runInAff)
  runtimeIdentity <- RuntimeId <$> Ref.new unit
  scope <- Ref.new Nothing
  spec <- Ref.new input.spec
  state <- Ref.new input.initialState
  stateUpdate <- Ref.new input.stateUpdate
  pure $ Runtime
    { bindings
    , fresh: freshRef
    , needsStatePublish
    , props: propsRef
    , runInAff: runInAffRef
    , runtimeId: runtimeIdentity
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
     , stateUpdate :: state -> View state -> Effect Unit
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
      authorities <- Ref.new Map.empty
      cleanups <- Ref.new Map.empty
      forks <- Ref.new Map.empty
      handlers <- Ref.new Map.empty
      subscriptions <- Ref.new Map.empty
      let scope = Scope { active, authorities, cleanups, forks, generation, handlers, subscriptions }
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
      Ref.write Map.empty current.authorities
      normalizeRuntimeState (Runtime state)
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
-- root identity. Slot registration, state reconciliation, authority claim,
-- prior-root fencing, replacement registration, and gate opening form one
-- synchronous runtime transaction.
managedStart
  :: forall props state action m
   . Binding state
  -> Maybe (Aff Unit)
  -> ( RuntimeId
       -> Int
       -> ForkId
       -> Maybe Token
       -> state
       -> Maybe
            { cancel :: Maybe Token
            , computation :: HaloM props state action m Unit
            , state :: state
            , token :: Token
            }
     )
  -> HaloM props state action m Unit
managedStart binding privateDelay claim = HaloM $ ReaderT \execution ->
  liftEffect do
    current <- isCurrent execution
    when current do
      let
        runtime@(Runtime runtimeState) = execution.runtime
        Scope scope = execution.scope
        brand = Task.bindingBrand binding
      oldState <- Ref.read runtimeState.state
      registerTaskBinding runtime binding oldState
      reconciled <- reconcileExecution execution oldState
      displacedRoots <- takeAndFenceTokens execution.scope reconciled.displaced
      fid <- ForkId <$> fresh runtime
      case
        claim runtimeState.runtimeId scope.generation fid
          (Map.lookup brand reconciled.authorities)
          reconciled.state
        of
        Nothing -> do
          Ref.write reconciled.authorities scope.authorities
          when reconciled.changed do
            writeAndPublish runtime execution.scope reconciled.state
          traverse_ requestCancel displacedRoots
        Just managed -> do
          prepared <- prepare execution.runInAff runtime execution.scope (ForkError fid)
            (withPrivateDelay privateDelay managed.computation)
            { onComplete: Ref.modify_ (Map.delete fid) scope.forks
            , onUnexpected: exitManaged runtime execution.scope binding managed.token
            }
          previousRoots <- takeAndFenceTokens execution.scope case managed.cancel of
            Nothing -> []
            Just token -> [ token ]
          let authorities = Map.insert brand managed.token reconciled.authorities
          Ref.write authorities scope.authorities
          Ref.write managed.state runtimeState.state
          Ref.modify_ (Map.insert fid prepared.root) scope.forks
          publishState runtime execution.scope managed.state
          traverse_ requestCancel (displacedRoots <> previousRoots)
          prepared.start

-- Commit a typed managed result only while the canonical focus and runtime
-- authority both contain the exact token. The root remains tracked until its
-- ordinary completion finalizer removes it.
managedComplete
  :: forall props state action m
   . Binding state
  -> Token
  -> (state -> Maybe state)
  -> HaloM props state action m Unit
managedComplete binding token transition = HaloM $ ReaderT \execution ->
  liftEffect do
    current <- isCurrent execution
    when current do
      let
        runtime@(Runtime runtimeState) = execution.runtime
        Scope scope = execution.scope
        brand = Task.bindingBrand binding
      authorities <- Ref.read scope.authorities
      case Map.lookup brand authorities of
        Just authoritative | Task.sameToken token authoritative -> do
          oldState <- Ref.read runtimeState.state
          case transition oldState of
            Nothing -> pure unit
            Just newState -> do
              Ref.write (Map.delete brand authorities) scope.authorities
              Ref.write newState runtimeState.state
              publishState runtime execution.scope newState
        _ -> pure unit

-- Stop managed work after atomically publishing its replacement state. A root
-- that resets itself is cancelled by unwinding its own fiber rather than trying
-- to join itself.
managedReset
  :: forall props state action m
   . Binding state
  -> ( RuntimeId
       -> Int
       -> Maybe Token
       -> state
       -> Maybe
            { cancel :: Maybe Token
            , state :: state
            }
     )
  -> HaloM props state action m Unit
managedReset binding transition = HaloM $ ReaderT \execution -> do
  current <- liftEffect $ isCurrent execution
  when current do
    cancellation <- liftEffect do
      let
        runtime@(Runtime runtimeState) = execution.runtime
        Scope scope = execution.scope
        brand = Task.bindingBrand binding
      oldState <- Ref.read runtimeState.state
      registerTaskBinding runtime binding oldState
      reconciled <- reconcileExecution execution oldState
      displacedRoots <- takeAndFenceTokens execution.scope reconciled.displaced
      case
        transition runtimeState.runtimeId scope.generation
          (Map.lookup brand reconciled.authorities)
          reconciled.state
        of
        Nothing -> do
          Ref.write reconciled.authorities scope.authorities
          when reconciled.changed do
            writeAndPublish runtime execution.scope reconciled.state
          traverse_ requestCancel displacedRoots
          pure Nothing
        Just next -> do
          root <- takeAndFenceTokens execution.scope case next.cancel of
            Nothing -> []
            Just token -> [ token ]
          let authorities = Map.delete brand reconciled.authorities
          Ref.write authorities scope.authorities
          Ref.write next.state runtimeState.state
          publishState runtime execution.scope next.state
          traverse_ requestCancel displacedRoots
          pure case root of
            [ managedRoot ] -> Just managedRoot
            _ -> Nothing
    traverse_
      ( \managedRoot ->
          if sameOwner execution.owner managedRoot then
            Aff.throwError scopeCancellationError
          else cancelRootAff managedRoot
      )
      cancellation

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
    { root: Root { fiber, owner }
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

type ReconciledState state =
  { authorities :: Map String Token
  , changed :: Boolean
  , displaced :: Array Token
  , state :: state
  }

registerTaskBinding
  :: forall props state action m
   . Runtime props state action m
  -> Binding state
  -> state
  -> Effect Unit
registerTaskBinding (Runtime runtime) binding componentState = do
  bindings <- Ref.read runtime.bindings
  let brand = Task.bindingBrand binding
  case Map.lookup brand bindings of
    Just existing -> do
      sameFocus <- Task.sameBindingFocus componentState existing binding
      unless sameFocus $ Exception.throw $
        "Halo task slot \"" <> brand <> "\" is already bound to a different state focus"
    Nothing -> do
      collision <- foldM
        ( \found existing -> case found of
            Just _ -> pure found
            Nothing -> do
              sameFocus <- Task.sameBindingFocus componentState existing binding
              pure if sameFocus then Just (Task.bindingBrand existing) else Nothing
        )
        Nothing
        (Map.values bindings)
      case collision of
        Just existingBrand -> Exception.throw $
          "Halo task slot \"" <> brand <> "\" overlaps state focus bound as \"" <> existingBrand <> "\""
        Nothing -> Ref.write (Map.insert brand binding bindings) runtime.bindings

reconcileExecution
  :: forall props state action m
   . Execution props state action m
  -> state
  -> Effect (ReconciledState state)
reconcileExecution execution componentState = do
  let
    Runtime runtime = execution.runtime
    Scope scope = execution.scope
  bindings <- Ref.read runtime.bindings
  authorities <- Ref.read scope.authorities
  pure $ reconcileBindings bindings authorities componentState

reconcileBindings
  :: forall state
   . Map String (Binding state)
  -> Map String Token
  -> state
  -> ReconciledState state
reconcileBindings bindings authorities componentState =
  foldl reconcileOne
    { authorities
    , changed: false
    , displaced: []
    , state: componentState
    }
    (Map.values bindings)
  where
  reconcileOne current binding =
    let
      brand = Task.bindingBrand binding
      result = Task.reconcileBinding binding (Map.lookup brand current.authorities) current.state
      nextAuthorities = case result.authority of
        Nothing -> Map.delete brand current.authorities
        Just token -> Map.insert brand token current.authorities
      displaced = case result.displaced of
        Nothing -> current.displaced
        Just token -> current.displaced <> [ token ]
    in
      { authorities: nextAuthorities
      , changed: current.changed || result.changed || case result.displaced of
          Nothing -> false
          Just _ -> true
      , displaced
      , state: result.state
      }

commitReconciledState
  :: forall props state action m
   . Execution props state action m
  -> state
  -> Effect Unit
commitReconciledState execution proposedState = do
  let
    runtime@(Runtime runtimeState) = execution.runtime
    Scope scope = execution.scope
  reconciled <- reconcileExecution execution proposedState
  displacedRoots <- takeAndFenceTokens execution.scope reconciled.displaced
  Ref.write reconciled.authorities scope.authorities
  Ref.write reconciled.state runtimeState.state
  publishState runtime execution.scope reconciled.state
  traverse_ requestCancel displacedRoots

normalizeRuntimeState :: forall props state action m. Runtime props state action m -> Effect Unit
normalizeRuntimeState (Runtime runtime) = do
  bindings <- Ref.read runtime.bindings
  currentState <- Ref.read runtime.state
  let reconciled = reconcileBindings bindings Map.empty currentState
  when reconciled.changed do
    Ref.write reconciled.state runtime.state
    Ref.write true runtime.needsStatePublish

exitManaged
  :: forall props state action m
   . Runtime props state action m
  -> Scope
  -> Binding state
  -> Token
  -> Effect Unit
exitManaged runtime@(Runtime runtimeState) scope@(Scope scopeState) binding token = do
  authorities <- Ref.read scopeState.authorities
  let brand = Task.bindingBrand binding
  case Map.lookup brand authorities of
    Just current | Task.sameToken token current -> do
      oldState <- Ref.read runtimeState.state
      case Task.clearBinding binding token oldState of
        Nothing -> pure unit
        Just newState -> do
          Ref.write (Map.delete brand authorities) scopeState.authorities
          Ref.write newState runtimeState.state
          publishState runtime scope newState
    _ -> pure unit

publishRuntimeState :: forall props state action m. Runtime props state action m -> Effect Unit
publishRuntimeState runtime@(Runtime runtimeState) = do
  needsPublish <- Ref.read runtimeState.needsStatePublish
  when needsPublish do
    Ref.write false runtimeState.needsStatePublish
    currentState <- Ref.read runtimeState.state
    activeScope <- Ref.read runtimeState.scope
    case activeScope of
      Nothing -> pure unit
      Just scope -> publishState runtime scope currentState

writeAndPublish
  :: forall props state action m
   . Runtime props state action m
  -> Scope
  -> state
  -> Effect Unit
writeAndPublish runtime@(Runtime runtimeState) scope componentState = do
  Ref.write componentState runtimeState.state
  publishState runtime scope componentState

publishState
  :: forall props state action m
   . Runtime props state action m
  -> Scope
  -> state
  -> Effect Unit
publishState (Runtime runtime) (Scope scope) componentState = do
  authorities <- Ref.read scope.authorities
  update <- Ref.read runtime.stateUpdate
  update componentState (Task.makeView componentState authorities)

takeAndFenceTokens :: Scope -> Array Token -> Effect (Array Root)
takeAndFenceTokens (Scope scope) = foldM takeOne []
  where
  takeOne roots token = do
    let fid = Task.tokenForkId token
    root <- Ref.modify'
      ( \forks ->
          { state: Map.delete fid forks
          , value: Map.lookup fid forks
          }
      )
      scope.forks
    traverse_ fenceRoot root
    pure $ roots <> case root of
      Nothing -> []
      Just managedRoot -> [ managedRoot ]

createOwner :: Effect Owner
createOwner = do
  alive <- Ref.new true
  pure $ Owner { alive }

fenceRoot :: Root -> Effect Unit
fenceRoot (Root root) = do
  let Owner owner = root.owner
  Ref.write false owner.alive

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
