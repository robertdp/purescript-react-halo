module React.Halo.Internal.Eval where

import Prelude

import Control.Applicative.Free (foldFreeAp)
import Control.Monad.Free (foldFree)
import Data.Either (either)
import Data.Foldable (traverse_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, ParAff, finally, parallel, sequential, throwError)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Halogen.Subscription (subscribe, unsubscribe)
import React.Halo.Internal.Control (HaloAp(..), HaloF(..), HaloM(..))
import React.Halo.Internal.State (HaloState(..))
import React.Halo.Internal.State as State
import React.Halo.Internal.Types (ForkId(..), Lifecycle(..), SubscriptionId(..))
import Unsafe.Reference (unsafeRefEq)

-- | Interprets `HaloM` into the base monad `Aff` for asynchronous effects.
evalHaloM :: forall props state action. HaloState props state action -> HaloM props state action Aff ~> Aff
evalHaloM hs (HaloM halo) = foldFree (evalHaloF hs) halo

-- | Interprets `HaloAp` into the base applicative `ParAff` for parallel effects.
evalHaloAp :: forall props state action. HaloState props state action -> HaloAp props state action Aff ~> ParAff
evalHaloAp hs (HaloAp halo) = foldFreeAp (parallel <<< evalHaloM hs) halo

-- | Interprets `HaloF` into the base monad `Aff`, keeping track of state in `HaloState`.
evalHaloF :: forall props state action. HaloState props state action -> HaloF props state action Aff ~> Aff
evalHaloF hs@(HaloState s) = case _ of
  Props k ->
    liftEffect do
      props <- Ref.read s.props
      pure (k props)
  State f ->
    liftEffect do
      state <- Ref.read s.state
      case f state of
        Tuple a state'
          | not unsafeRefEq state state' -> do
              Ref.write state' s.state
              s.update state'
              pure a
          | otherwise -> pure a
  Subscribe sub k ->
    liftEffect do
      sid <- State.fresh SubscriptionId hs
      unlessM (Ref.read s.finalized) do
        canceller <- subscribe (sub sid) (handleAction hs)
        Ref.modify_ (Map.insert sid canceller) s.subscriptions
      pure (k sid)
  Unsubscribe sid a ->
    liftEffect do
      subscription <- Map.lookup sid <$> Ref.read s.subscriptions
      traverse_ unsubscribe subscription
      pure a
  Lift m ->
    m
  Par p ->
    sequential (evalHaloAp hs p)
  Fork fh k ->
    liftEffect do
      fid <- State.fresh ForkId hs
      doneRef <- Ref.new false
      fiber <-
        Aff.launchAff
          $ finally
              ( liftEffect do
                  Ref.modify_ (Map.delete fid) s.forks
                  Ref.write true doneRef
              )
              (evalHaloM hs fh)
      unlessM (Ref.read doneRef) do
        Ref.modify_ (Map.insert fid fiber) s.forks
      pure (k fid)
  Kill fid a -> do
    forks <- liftEffect (Ref.read s.forks)
    traverse_ (Aff.killFiber (Aff.error "Cancelled")) (Map.lookup fid forks)
    pure a

-- | A simpler interface for building the components eval function. The main lifecycle events map directly into
-- | actions, so only the action handling logic needs to be written using `HaloM`.
type EvalSpec props state action m =
  { handleAction :: action -> HaloM props state action m Unit
  , initialize :: Maybe action
  , update :: props -> Maybe action
  , finalize :: Maybe action
  }

-- | The empty `EvalSpec`.
defaultEval :: forall props action state m. EvalSpec props state action m
defaultEval =
  { handleAction: \_ -> pure unit
  , initialize: Nothing
  , update: \_ -> Nothing
  , finalize: Nothing
  }

-- | Given an `EvalSpec` builder, it will return an eval function.
mkEval :: forall props state action m. EvalSpec props state action m -> Lifecycle props action -> HaloM props state action m Unit
mkEval eval = case _ of
  Initialize -> traverse_ eval.handleAction $ eval.initialize
  Update props -> traverse_ eval.handleAction $ eval.update props
  Action action -> eval.handleAction action
  Finalize -> traverse_ eval.handleAction eval.finalize

-- | Simple way to run Aff logic asynchronously, while bringing errors back into Effect.
runAff :: Aff Unit -> Effect Unit
runAff = Aff.runAff_ (either throwError pure)

runInitialize :: forall props state action. HaloState props action state -> Effect Unit
runInitialize hs@(HaloState s) =
  runAff $ evalHaloM hs $ s.eval Initialize

handleUpdate :: forall props state action. HaloState props action state -> props -> Effect Unit
handleUpdate hs@(HaloState s) newProps = do
  prevProps <- Ref.read s.props
  unless (unsafeRefEq newProps prevProps) do
    Ref.write newProps s.props
    runAff $ evalHaloM hs $ s.eval $ Update prevProps

handleAction :: forall props state action. HaloState props state action -> action -> Effect Unit
handleAction hs@(HaloState s) action =
  unlessM (Ref.read s.finalized) do
    runAff $ evalHaloM hs $ s.eval $ Action action

runFinalize :: forall props state action. HaloState props state action -> Effect Unit
runFinalize hs@(HaloState s) = do
  Ref.write true s.finalized
  subscriptions <- Ref.modify' (\s' -> { state: Map.empty, value: s' }) s.subscriptions
  traverse_ unsubscribe (Map.values subscriptions)
  forks <- Ref.modify' (\s' -> { state: Map.empty, value: s' }) s.forks
  traverse_ (runAff <<< Aff.killFiber (Aff.error "Cancelled")) (Map.values forks)
  runAff $ evalHaloM hs $ s.eval Finalize
