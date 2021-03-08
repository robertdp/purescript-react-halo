module React.Halo.Internal.Eval where

import Prelude
import Control.Applicative.Free (foldFreeAp)
import Control.Monad.Free (foldFree)
import Data.Either (either)
import Data.Foldable (sequence_, traverse_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, ParAff, finally, parallel, sequential, throwError)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import FRP.Event as Event
import React.Halo.Internal.Control (HaloAp(..), HaloF(..), HaloM(..))
import React.Halo.Internal.State (HaloState(..))
import React.Halo.Internal.State as State
import React.Halo.Internal.Types (ForkId(..), Lifecycle(..), SubscriptionId(..))
import Unsafe.Reference (unsafeRefEq)

-- | Interprets `HaloM` into the base monad `Aff` for asynchronous effects.
evalHaloM :: forall ctx state action. HaloState ctx state action -> HaloM ctx state action Aff ~> Aff
evalHaloM hs (HaloM halo) = foldFree (evalHaloF hs) halo

-- | Interprets `HaloAp` into the base applicative `ParAff` for parallel effects.
evalHaloAp :: forall ctx state action. HaloState ctx state action -> HaloAp ctx state action Aff ~> ParAff
evalHaloAp hs (HaloAp halo) = foldFreeAp (parallel <<< evalHaloM hs) halo

-- | Interprets `HaloF` into the base monad `Aff`, keeping track of state in `HaloState`.
evalHaloF :: forall ctx state action. HaloState ctx state action -> HaloF ctx state action Aff ~> Aff
evalHaloF hs@(HaloState s) = case _ of
  Context k ->
    liftEffect do
      context <- Ref.read s.context
      pure (k context)
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
        canceller <- Event.subscribe (sub sid) (handleAction hs)
        Ref.modify_ (Map.insert sid canceller) s.subscriptions
      pure (k sid)
  Unsubscribe sid a ->
    liftEffect do
      canceller <- Map.lookup sid <$> Ref.read s.subscriptions
      sequence_ canceller
      pure a
  Lift m -> m
  Par p -> sequential (evalHaloAp hs p)
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
type EvalSpec ctx state action m
  = { onInitialize :: ctx -> Maybe action
    , onUpdate :: ctx -> ctx -> Maybe action
    , onAction :: action -> HaloM ctx state action m Unit
    , onFinalize :: Maybe action
    }

-- | The empty `EvalSpec`.
defaultEval :: forall ctx action state m. EvalSpec ctx state action m
defaultEval =
  { onInitialize: \_ -> Nothing
  , onUpdate: \_ _ -> Nothing
  , onAction: \_ -> pure unit
  , onFinalize: Nothing
  }

-- | Given an `EvalSpec` builder, it will return an eval function.
mkEval ::
  forall ctx state action m.
  (EvalSpec ctx state action m -> EvalSpec ctx state action m) ->
  Lifecycle ctx action ->
  HaloM ctx state action m Unit
mkEval f = case _ of
  Initialize ctx -> traverse_ eval.onAction $ eval.onInitialize ctx
  Update old new -> traverse_ eval.onAction $ eval.onUpdate old new
  Action action -> eval.onAction action
  Finalize -> traverse_ eval.onAction eval.onFinalize
  where
  eval = f defaultEval

-- | Simple way to run Aff logic asynchronously, while bringing errors back into Effect.
runAff :: Aff Unit -> Effect Unit
runAff = Aff.runAff_ (either throwError pure)

runInitialize :: forall ctx state action. HaloState ctx action state -> Effect Unit
runInitialize hs@(HaloState s) = do
  context <- Ref.read s.context
  runAff $ evalHaloM hs $ s.eval $ Initialize context

handleUpdate :: forall ctx state action. HaloState ctx action state -> ctx -> Effect Unit
handleUpdate hs@(HaloState s) context = do
  context' <- Ref.read s.context
  unless (unsafeRefEq context context') do
    Ref.write context s.context
    runAff $ evalHaloM hs $ s.eval $ Update context' context

handleAction :: forall ctx state action. HaloState ctx state action -> action -> Effect Unit
handleAction hs@(HaloState s) action = do
  unlessM (Ref.read s.finalized) do
    runAff $ evalHaloM hs $ s.eval $ Action action

runFinalize :: forall ctx state action. HaloState ctx state action -> Effect Unit
runFinalize hs@(HaloState s) = do
  Ref.write true s.finalized
  subscriptions <- Ref.modify' (\s' -> { state: Map.empty, value: s' }) s.subscriptions
  sequence_ (Map.values subscriptions)
  forks <- Ref.modify' (\s' -> { state: Map.empty, value: s' }) s.forks
  traverse_ (runAff <<< Aff.killFiber (Aff.error "Cancelled")) (Map.values forks)
  runAff $ evalHaloM hs $ s.eval Finalize
