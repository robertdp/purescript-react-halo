module React.Halo.Internal.Eval where

import Prelude
import Control.Alt ((<|>))
import Control.Applicative.Free (hoistFreeAp, retractFreeAp)
import Control.Monad.Free (foldFree)
import Data.Either (either)
import Data.Foldable (sequence_, traverse_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, ParAff, finally, parallel, sequential, throwError)
import Effect.Aff as Aff
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import React.Halo.Internal.Control (HaloAp(..), HaloF(..), HaloM(..))
import React.Halo.Internal.State (HaloState(..))
import React.Halo.Internal.State as State
import React.Halo.Internal.Types (ForkId(..), Lifecycle(..), SubscriptionId(..))
import Unsafe.Reference (unsafeRefEq)
import Wire.Event as Event

-- | Interprets `HaloM` into the base monad `Aff`.
evalHaloM :: forall props state action. HaloState props state action -> HaloM props state action Aff ~> Aff
evalHaloM hs@(HaloState s) (HaloM halo) = foldFree (evalHaloF hs) halo

-- | Interprets `HaloAp` into the base applicative `ParAff`.
evalHaloAp :: forall props state action. HaloState props state action -> HaloAp props state action Aff ~> ParAff
evalHaloAp hs@(HaloState s) (HaloAp halo) = retractFreeAp $ hoistFreeAp (parallel <<< evalHaloM hs) halo

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
        canceller <- Event.subscribe (sub sid) (handleAction hs)
        Ref.modify_ (Map.insert sid canceller) s.subscriptions
      pure (k sid)
  Unsubscribe sid a ->
    liftEffect do
      canceller <- Map.lookup sid <$> Ref.read s.subscriptions
      sequence_ canceller
      pure a
  Lift m -> liftAff m
  Par p -> sequential $ evalHaloAp hs p
  Race p p' -> sequential $ evalHaloAp hs p <|> evalHaloAp hs p'
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
type EvalSpec props state action m
  = { onInitialize :: props -> Maybe action
    , onUpdate :: props -> props -> Maybe action
    , onAction :: action -> HaloM props state action m Unit
    , onFinalize :: Maybe action
    }

-- | The empty `EvalSpec`.
defaultEval :: forall props action state m. EvalSpec props state action m
defaultEval =
  { onInitialize: \_ -> Nothing
  , onUpdate: \_ _ -> Nothing
  , onAction: \_ -> pure unit
  , onFinalize: Nothing
  }

-- | Given an `EvalSpec` builder, it will return an eval function.
makeEval ::
  forall m action state props.
  (EvalSpec props state action m -> EvalSpec props state action m) ->
  Lifecycle props action -> HaloM props state action m Unit
makeEval f = case _ of
  Initialize props -> traverse_ eval.onAction $ eval.onInitialize props
  Update old new -> traverse_ eval.onAction $ eval.onUpdate old new
  Action action -> eval.onAction action
  Finalize -> traverse_ eval.onAction eval.onFinalize
  where
  eval = f defaultEval

-- | Simple way to run Aff logic asynchronously, while bringing errors back into Effect.
runAff :: Aff Unit -> Effect Unit
runAff = Aff.runAff_ (either throwError pure)

runInitialize :: forall props state action. HaloState props action state -> Effect Unit
runInitialize hs@(HaloState s) = do
  props <- Ref.read s.props
  runAff $ evalHaloM hs $ s.eval $ Initialize props

handleUpdate :: forall props state action. HaloState props action state -> props -> Effect Unit
handleUpdate hs@(HaloState s) props = do
  props' <- Ref.read s.props
  unless (unsafeRefEq props props') do
    Ref.write props s.props
    runAff $ evalHaloM hs $ s.eval $ Update props' props

handleAction :: forall props state action. HaloState props state action -> action -> Effect Unit
handleAction hs@(HaloState s) action = do
  unlessM (Ref.read s.finalized) do
    runAff $ evalHaloM hs $ s.eval $ Action action

runFinalize :: forall props state action. HaloState props state action -> Effect Unit
runFinalize hs@(HaloState s) = do
  Ref.write true s.finalized
  subscriptions <- Ref.modify' (\s' -> { state: Map.empty, value: s' }) s.subscriptions
  sequence_ (Map.values subscriptions)
  forks <- Ref.modify' (\s' -> { state: Map.empty, value: s' }) s.forks
  traverse_ (runAff <<< Aff.killFiber (Aff.error "Cancelled")) (Map.values forks)
  runAff $ evalHaloM hs $ s.eval Finalize
