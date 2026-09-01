module React.Halo.Internal.Runtime
  ( HaloM
  , Runtime
  , RuntimeSpec
  , activate
  , createRuntime
  , deactivate
  , dispatch
  , fork
  , kill
  , props
  , runForTest
  , subscribe
  , subscribe'
  , syncSpec
  , unsubscribe
  , updateProps
  ) where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State.Class (class MonadState)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (and, foldl, traverse_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Error, Fiber)
import Effect.Aff as Aff
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.AVar as EffectAVar
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen.Subscription (Emitter, Subscription)
import Halogen.Subscription as HS
import React.Halo.Internal.Types (Activity(..), ErrorContext(..), ForkId(..), Lifecycle(..), SubscriptionId(..), TaskPolicy(..), emptyActivity)
import Unsafe.Reference (unsafeRefEq)

-- | The direct Halo evaluator. Its environment is intentionally private so a
-- | computation can only obtain the capabilities exported by `React.Halo`.
newtype HaloM props state action key a = HaloM
  (ReaderT (Execution props state action key) Aff a)

derive newtype instance functorHaloM :: Functor (HaloM props state action key)
derive newtype instance applyHaloM :: Apply (HaloM props state action key)
derive newtype instance applicativeHaloM :: Applicative (HaloM props state action key)
derive newtype instance bindHaloM :: Bind (HaloM props state action key)
derive newtype instance monadHaloM :: Monad (HaloM props state action key)
derive newtype instance monadEffectHaloM :: MonadEffect (HaloM props state action key)
derive newtype instance monadAffHaloM :: MonadAff (HaloM props state action key)

type RuntimeSpec props state action key =
  { eval :: Lifecycle props action -> HaloM props state action key Unit
  , onError :: ErrorContext props action -> Error -> Effect Unit
  , schedule :: action -> TaskPolicy key
  }

newtype Runtime props state action key = Runtime
  { activityUpdate :: Ref (Activity key -> Effect Unit)
  , fresh :: Ref Int
  , props :: Ref props
  , scope :: Ref (Maybe (Scope props state action key))
  , spec :: Ref (RuntimeSpec props state action key)
  , state :: Ref state
  , stateUpdate :: Ref (state -> Effect Unit)
  }

newtype Scope :: Type -> Type -> Type -> Type -> Type
newtype Scope props state action key = Scope
  { active :: Ref Boolean
  , every :: Ref (Map Int (Root props state action key))
  , generation :: Int
  , roots :: Ref (Map Int (Root props state action key))
  , subscriptions :: Ref (Map SubscriptionId Subscription)
  , tasks :: Ref (Map key (TaskSlot props state action key))
  }

type TaskSlot :: Type -> Type -> Type -> Type -> Type
type TaskSlot props state action key =
  { queued :: Array action
  , running :: Map Int (Root props state action key)
  }

newtype Owner :: Type -> Type -> Type -> Type -> Type
newtype Owner props state action key = Owner
  { alive :: Ref Boolean
  , children :: Ref (Map ForkId (Root props state action key))
  , lineage :: Array (Ref Boolean)
  }

newtype Root :: Type -> Type -> Type -> Type -> Type
newtype Root props state action key = Root
  { fiber :: Fiber Unit
  , owner :: Owner props state action key
  }

type Execution props state action key =
  { context :: ErrorContext props action
  , owner :: Owner props state action key
  , runtime :: Runtime props state action key
  , scope :: Scope props state action key
  }

type Prepared :: Type -> Type -> Type -> Type -> Type
type Prepared props state action key =
  { root :: Root props state action key
  , start :: Effect Unit
  }

instance monadStateHaloM :: MonadState state (HaloM props state action key) where
  state f = HaloM do
    execution <- ask
    liftEffect do
      current <- isCurrent execution
      if current then do
        let Runtime runtime = execution.runtime
        oldState <- Ref.read runtime.state
        let Tuple result newState = f oldState
        unless (unsafeRefEq oldState newState) do
          Ref.write newState runtime.state
          update <- Ref.read runtime.stateUpdate
          update newState
        pure result
      else do
        let Runtime runtime = execution.runtime
        Tuple result _ <- f <$> Ref.read runtime.state
        pure result

createRuntime
  :: forall props state action key
   . { activityUpdate :: Activity key -> Effect Unit
     , initialProps :: props
     , initialState :: state
     , spec :: RuntimeSpec props state action key
     , stateUpdate :: state -> Effect Unit
     }
  -> Effect (Runtime props state action key)
createRuntime input = do
  activityUpdate <- Ref.new input.activityUpdate
  freshRef <- Ref.new 0
  propsRef <- Ref.new input.initialProps
  scope <- Ref.new Nothing
  spec <- Ref.new input.spec
  state <- Ref.new input.initialState
  stateUpdate <- Ref.new input.stateUpdate
  pure $ Runtime
    { activityUpdate
    , fresh: freshRef
    , props: propsRef
    , scope
    , spec
    , state
    , stateUpdate
    }

syncSpec
  :: forall props state action key
   . Runtime props state action key
  -> { activityUpdate :: Activity key -> Effect Unit
     , spec :: RuntimeSpec props state action key
     , stateUpdate :: state -> Effect Unit
     }
  -> Effect Unit
syncSpec (Runtime runtime) input = do
  Ref.write input.activityUpdate runtime.activityUpdate
  Ref.write input.spec runtime.spec
  Ref.write input.stateUpdate runtime.stateUpdate

activate :: forall props state action key. Ord key => Runtime props state action key -> Effect Unit
activate runtime@(Runtime state) = do
  activeScope <- Ref.read state.scope
  case activeScope of
    Just _ -> pure unit
    Nothing -> do
      generation <- fresh runtime
      active <- Ref.new true
      every <- Ref.new Map.empty
      roots <- Ref.new Map.empty
      subscriptions <- Ref.new Map.empty
      tasks <- Ref.new Map.empty
      let scope = Scope { active, every, generation, roots, subscriptions, tasks }
      Ref.write (Just scope) state.scope
      spec <- Ref.read state.spec
      startLifecycle runtime scope ActivationError (spec.eval Activate)

deactivate :: forall props state action key. Ord key => Runtime props state action key -> Effect Unit
deactivate runtime@(Runtime state) = do
  activeScope <- Ref.read state.scope
  case activeScope of
    Nothing -> pure unit
    Just (Scope current) -> do
      Ref.write false current.active
      Ref.write Nothing state.scope

      roots <- takeRef current.roots Map.empty
      every <- takeRef current.every Map.empty
      tasks <- takeRef current.tasks Map.empty
      subscriptions <- takeRef current.subscriptions Map.empty

      publishActivity runtime emptyActivity
      traverse_ HS.unsubscribe (Map.values subscriptions)
      traverse_ cancelRoot (Map.values roots)
      traverse_ cancelRoot (Map.values every)
      traverse_ (traverse_ cancelRoot <<< Map.values <<< _.running) (Map.values tasks)

updateProps
  :: forall props state action key
   . Ord key
  => Runtime props state action key
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
          startLifecycle runtime scope (UpdateError previousProps) (spec.eval (Update previousProps))
      )
      activeScope

dispatch
  :: forall props state action key
   . Ord key
  => Runtime props state action key
  -> action
  -> Effect Unit
dispatch runtime@(Runtime state) action = do
  activeScope <- Ref.read state.scope
  traverse_ (\scope -> dispatchToScope runtime scope action) activeScope

dispatchToScope
  :: forall props state action key
   . Ord key
  => Runtime props state action key
  -> Scope props state action key
  -> action
  -> Effect Unit
dispatchToScope runtime@(Runtime state) scope action = do
  current <- isScopeCurrent runtime scope
  when current do
    spec <- Ref.read state.spec
    schedule runtime scope (spec.schedule action) action

props :: forall props state action key. HaloM props state action key props
props = HaloM do
  execution <- ask
  let Runtime runtime = execution.runtime
  liftEffect $ Ref.read runtime.props

subscribe
  :: forall props state action key
   . Ord key
  => Emitter action
  -> HaloM props state action key SubscriptionId
subscribe = subscribe' <<< const

subscribe'
  :: forall props state action key
   . Ord key
  => (SubscriptionId -> Emitter action)
  -> HaloM props state action key SubscriptionId
subscribe' makeEmitter = HaloM do
  execution <- ask
  liftEffect do
    sid <- SubscriptionId <$> fresh execution.runtime
    current <- isCurrent execution
    when current do
      let Scope scope = execution.scope
      subscription <- HS.subscribe (makeEmitter sid) (dispatchToScope execution.runtime execution.scope)
      Ref.modify_ (Map.insert sid subscription) scope.subscriptions
    pure sid

unsubscribe
  :: forall props state action key
   . SubscriptionId
  -> HaloM props state action key Unit
unsubscribe sid = HaloM do
  execution <- ask
  liftEffect do
    current <- isCurrent execution
    when current do
      let Scope scope = execution.scope
      subscription <- Ref.modify'
        ( \subscriptions ->
            { state: Map.delete sid subscriptions
            , value: Map.lookup sid subscriptions
            }
        )
        scope.subscriptions
      traverse_ HS.unsubscribe subscription

fork
  :: forall props state action key
   . HaloM props state action key Unit
  -> HaloM props state action key ForkId
fork child = HaloM do
  execution <- ask
  liftEffect do
    fid <- ForkId <$> fresh execution.runtime
    current <- isCurrent execution
    when current do
      prepared <- prepare (Just execution.owner) execution.runtime execution.scope execution.context child \_ -> do
        let Owner parent = execution.owner
        Ref.modify_ (Map.delete fid) parent.children
      let Owner parent = execution.owner
      Ref.modify_ (Map.insert fid prepared.root) parent.children
      prepared.start
    pure fid

kill
  :: forall props state action key
   . ForkId
  -> HaloM props state action key Unit
kill fid = HaloM do
  execution <- ask
  let Owner parent = execution.owner
  child <- liftEffect $ Ref.modify'
    ( \children ->
        { state: Map.delete fid children
        , value: Map.lookup fid children
        }
    )
    parent.children
  traverse_ (liftAff <<< cancelRootAff) child

-- | Internal test seam: run one computation in a currently active scope. This
-- | is not re-exported by `React.Halo`.
runForTest
  :: forall props state action key
   . Ord key
  => Runtime props state action key
  -> ErrorContext props action
  -> HaloM props state action key Unit
  -> Effect Unit
runForTest runtime@(Runtime state) context computation = do
  activeScope <- Ref.read state.scope
  traverse_ (\scope -> startLifecycle runtime scope context computation) activeScope

startLifecycle
  :: forall props state action key
   . Ord key
  => Runtime props state action key
  -> Scope props state action key
  -> ErrorContext props action
  -> HaloM props state action key Unit
  -> Effect Unit
startLifecycle runtime scope@(Scope current) context computation = do
  runId <- fresh runtime
  prepared <- prepare Nothing runtime scope context computation \_ ->
    Ref.modify_ (Map.delete runId) current.roots
  Ref.modify_ (Map.insert runId prepared.root) current.roots
  prepared.start

schedule
  :: forall props state action key
   . Ord key
  => Runtime props state action key
  -> Scope props state action key
  -> TaskPolicy key
  -> action
  -> Effect Unit
schedule runtime scope@(Scope current) policy action = case policy of
  Every -> do
    runId <- fresh runtime
    prepared <- prepare Nothing runtime scope (ActionError action) (evalAction runtime action) \_ -> do
      Ref.modify_ (Map.delete runId) current.every
      notifyActivity runtime scope
    Ref.modify_ (Map.insert runId prepared.root) current.every
    notifyActivity runtime scope
    prepared.start
  Restartable key -> do
    tasks <- Ref.read current.tasks
    let previous = maybe mempty (Map.values <<< _.running) (Map.lookup key tasks)
    Ref.modify_ (Map.insert key { running: Map.empty, queued: [] }) current.tasks
    traverse_ cancelRoot previous
    startKeyed runtime scope key action
  Drop key -> do
    tasks <- Ref.read current.tasks
    let busy = maybe false (\slot -> not Map.isEmpty slot.running || not Array.null slot.queued) (Map.lookup key tasks)
    unless busy $ startKeyed runtime scope key action
  Enqueue key -> enqueueOrStart runtime scope key action false
  KeepLatest key -> enqueueOrStart runtime scope key action true

startKeyed
  :: forall props state action key
   . Ord key
  => Runtime props state action key
  -> Scope props state action key
  -> key
  -> action
  -> Effect Unit
startKeyed runtime scope@(Scope current) key action = do
  runId <- fresh runtime
  prepared <- prepare Nothing runtime scope (ActionError action) (evalAction runtime action) \_ ->
    completeKeyed runtime scope key runId
  Ref.modify_ (Map.alter (Just <<< addRun runId prepared.root <<< maybe emptySlot identity) key) current.tasks
  notifyActivity runtime scope
  prepared.start

enqueueOrStart
  :: forall props state action key
   . Ord key
  => Runtime props state action key
  -> Scope props state action key
  -> key
  -> action
  -> Boolean
  -> Effect Unit
enqueueOrStart runtime scope@(Scope current) key action keepOnlyLatest = do
  tasks <- Ref.read current.tasks
  case Map.lookup key tasks of
    Just slot | not Map.isEmpty slot.running -> do
      let queued = if keepOnlyLatest then [ action ] else Array.snoc slot.queued action
      Ref.modify_ (Map.insert key (slot { queued = queued })) current.tasks
      notifyActivity runtime scope
    _ -> startKeyed runtime scope key action

completeKeyed
  :: forall props state action key
   . Ord key
  => Runtime props state action key
  -> Scope props state action key
  -> key
  -> Int
  -> Effect Unit
completeKeyed runtime scope@(Scope current) key runId = do
  tasks <- Ref.read current.tasks
  case Map.lookup key tasks of
    Nothing -> pure unit
    Just slot | not (Map.member runId slot.running) -> pure unit
    Just slot -> do
      let running = Map.delete runId slot.running
      case Array.uncons slot.queued of
        Just { head, tail } | Map.isEmpty running -> do
          Ref.modify_ (Map.insert key { running, queued: tail }) current.tasks
          notifyActivity runtime scope
          startKeyed runtime scope key head
        _ -> do
          if Map.isEmpty running && Array.null slot.queued then
            Ref.modify_ (Map.delete key) current.tasks
          else
            Ref.modify_ (Map.insert key (slot { running = running })) current.tasks
          notifyActivity runtime scope

prepare
  :: forall props state action key
   . Maybe (Owner props state action key)
  -> Runtime props state action key
  -> Scope props state action key
  -> ErrorContext props action
  -> HaloM props state action key Unit
  -> (Owner props state action key -> Effect Unit)
  -> Effect (Prepared props state action key)
prepare parent runtime scope context computation onComplete = do
  owner <- createOwner parent
  gate <- EffectAVar.empty
  fiber <- Aff.launchAff $ do
    void $ AVar.take gate
    Aff.finally
      (closeOwner owner *> liftEffect (onComplete owner))
      do
        outcome <- Aff.attempt $ runHaloM { context, owner, runtime, scope } computation
        case outcome of
          Left error -> do
            current <- liftEffect $ isCurrent { context, owner, runtime, scope }
            when current do
              let Runtime state = runtime
              spec <- liftEffect $ Ref.read state.spec
              liftEffect $ spec.onError context error
          Right _ -> pure unit
  let root = Root { fiber, owner }
  pure
    { root
    , start: Aff.launchAff_ (AVar.put unit gate)
    }

runHaloM
  :: forall props state action key a
   . Execution props state action key
  -> HaloM props state action key a
  -> Aff a
runHaloM execution (HaloM computation) = runReaderT computation execution

evalAction
  :: forall props state action key
   . Runtime props state action key
  -> action
  -> HaloM props state action key Unit
evalAction (Runtime runtime) action = HaloM do
  spec <- liftEffect $ Ref.read runtime.spec
  let HaloM computation = spec.eval (Action action)
  computation

createOwner
  :: forall props state action key
   . Maybe (Owner props state action key)
  -> Effect (Owner props state action key)
createOwner parent = do
  alive <- Ref.new true
  children <- Ref.new Map.empty
  let
    ancestors = case parent of
      Just (Owner owner) -> owner.lineage
      Nothing -> []
  pure $ Owner { alive, children, lineage: Array.cons alive ancestors }

closeOwner :: forall props state action key. Owner props state action key -> Aff Unit
closeOwner (Owner owner) = do
  children <- liftEffect do
    Ref.write false owner.alive
    takeRef owner.children Map.empty
  traverse_ cancelRootAff (Map.values children)

cancelRoot :: forall props state action key. Root props state action key -> Effect Unit
cancelRoot root@(Root current) = do
  let Owner owner = current.owner
  -- Fence commits synchronously. Fiber cancellation is asynchronous and cannot
  -- stop external effects that have already happened.
  Ref.write false owner.alive
  Aff.launchAff_ (cancelRootAff root)

cancelRootAff :: forall props state action key. Root props state action key -> Aff Unit
cancelRootAff (Root root) = do
  let Owner owner = root.owner
  liftEffect $ Ref.write false owner.alive
  Aff.killFiber (Aff.error "Halo scope cancelled") root.fiber

isCurrent
  :: forall props state action key
   . Execution props state action key
  -> Effect Boolean
isCurrent execution = do
  let Owner owner = execution.owner
  let Scope scope = execution.scope
  let Runtime runtime = execution.runtime
  ownerAlive <- and <$> traverse Ref.read owner.lineage
  scopeActive <- Ref.read scope.active
  activeScope <- Ref.read runtime.scope
  pure $ ownerAlive && scopeActive && case activeScope of
    Just (Scope active) -> active.generation == scope.generation
    Nothing -> false

notifyActivity
  :: forall props state action key
   . Ord key
  => Runtime props state action key
  -> Scope props state action key
  -> Effect Unit
notifyActivity runtime@(Runtime state) scope@(Scope current) = do
  active <- isScopeCurrent runtime scope
  when active do
    every <- Ref.read current.every
    tasks <- Ref.read current.tasks
    let
      counts slot =
        { running: Map.size slot.running
        , queued: Array.length slot.queued
        }
      byKey = map counts tasks
      keyed = foldl addCounts { running: 0, queued: 0 } (Map.values byKey)
      activity = Activity
        { total: keyed { running = keyed.running + Map.size every }
        , byKey
        }
    update <- Ref.read state.activityUpdate
    update activity

publishActivity
  :: forall props state action key
   . Runtime props state action key
  -> Activity key
  -> Effect Unit
publishActivity (Runtime runtime) activity = do
  update <- Ref.read runtime.activityUpdate
  update activity

isScopeCurrent
  :: forall props state action key
   . Runtime props state action key
  -> Scope props state action key
  -> Effect Boolean
isScopeCurrent (Runtime runtime) (Scope scope) = do
  scopeActive <- Ref.read scope.active
  activeScope <- Ref.read runtime.scope
  pure $ scopeActive && case activeScope of
    Just (Scope active) -> active.generation == scope.generation
    Nothing -> false

addCounts :: { running :: Int, queued :: Int } -> { running :: Int, queued :: Int } -> { running :: Int, queued :: Int }
addCounts left right =
  { running: left.running + right.running
  , queued: left.queued + right.queued
  }

emptySlot :: forall props state action key. TaskSlot props state action key
emptySlot = { queued: [], running: Map.empty }

addRun
  :: forall props state action key
   . Int
  -> Root props state action key
  -> TaskSlot props state action key
  -> TaskSlot props state action key
addRun runId root slot = slot { running = Map.insert runId root slot.running }

fresh :: forall props state action key. Runtime props state action key -> Effect Int
fresh (Runtime runtime) = Ref.modify' (\value -> { state: value + 1, value }) runtime.fresh

takeRef :: forall a. Ref a -> a -> Effect a
takeRef ref replacement = Ref.modify' (\value -> { state: replacement, value }) ref
