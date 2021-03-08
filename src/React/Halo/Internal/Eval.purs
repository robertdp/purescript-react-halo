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
evalHaloM :: forall props ctx state action. HaloState props ctx state action -> HaloM props ctx state action Aff ~> Aff
evalHaloM hs (HaloM halo) = foldFree (evalHaloF hs) halo

-- | Interprets `HaloAp` into the base applicative `ParAff` for parallel effects.
evalHaloAp :: forall props ctx state action. HaloState props ctx state action -> HaloAp props ctx state action Aff ~> ParAff
evalHaloAp hs (HaloAp halo) = foldFreeAp (parallel <<< evalHaloM hs) halo

-- | Interprets `HaloF` into the base monad `Aff`, keeping track of state in `HaloState`.
evalHaloF :: forall props ctx state action. HaloState props ctx state action -> HaloF props ctx state action Aff ~> Aff
evalHaloF hs@(HaloState s) = case _ of
  Props k ->
    liftEffect do
      props <- Ref.read s.props
      pure (k props)
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
type EvalSpec props ctx state action m
  = { onInitialize :: { props :: props, context :: ctx } -> Maybe action
    , onUpdate :: { props :: props, context :: ctx } -> { props :: props, context :: ctx } -> Maybe action
    , onAction :: action -> HaloM props ctx state action m Unit
    , onFinalize :: Maybe action
    }

-- | The empty `EvalSpec`.
defaultEval :: forall props ctx action state m. EvalSpec props ctx state action m
defaultEval =
  { onInitialize: \_ -> Nothing
  , onUpdate: \_ _ -> Nothing
  , onAction: \_ -> pure unit
  , onFinalize: Nothing
  }

-- | Given an `EvalSpec` builder, it will return an eval function.
mkEval ::
  forall props ctx state action m.
  (EvalSpec props ctx state action m -> EvalSpec props ctx state action m) ->
  Lifecycle props ctx action ->
  HaloM props ctx state action m Unit
mkEval f = case _ of
  Initialize init -> traverse_ eval.onAction $ eval.onInitialize init
  Update old new -> traverse_ eval.onAction $ eval.onUpdate old new
  Action action -> eval.onAction action
  Finalize -> traverse_ eval.onAction eval.onFinalize
  where
  eval = f defaultEval

-- | Simple way to run Aff logic asynchronously, while bringing errors back into Effect.
runAff :: Aff Unit -> Effect Unit
runAff = Aff.runAff_ (either throwError pure)

runInitialize :: forall props ctx state action. HaloState props ctx action state -> Effect Unit
runInitialize hs@(HaloState s) = do
  props <- Ref.read s.props
  context <- Ref.read s.context
  runAff $ evalHaloM hs $ s.eval $ Initialize { props, context }

handleUpdate :: forall props ctx state action. HaloState props ctx action state -> props -> ctx -> Effect Unit
handleUpdate hs@(HaloState s) props context = do
  props' <- Ref.read s.props
  context' <- Ref.read s.context
  unless (unsafeRefEq props props' && unsafeRefEq context context') do
    Ref.write props s.props
    Ref.write context s.context
    runAff $ evalHaloM hs $ s.eval $ Update { props: props', context: context' } { props, context }

handleAction :: forall props ctx state action. HaloState props ctx state action -> action -> Effect Unit
handleAction hs@(HaloState s) action = do
  unlessM (Ref.read s.finalized) do
    runAff $ evalHaloM hs $ s.eval $ Action action

runFinalize :: forall props ctx state action. HaloState props ctx state action -> Effect Unit
runFinalize hs@(HaloState s) = do
  Ref.write true s.finalized
  subscriptions <- Ref.modify' (\s' -> { state: Map.empty, value: s' }) s.subscriptions
  sequence_ (Map.values subscriptions)
  forks <- Ref.modify' (\s' -> { state: Map.empty, value: s' }) s.forks
  traverse_ (runAff <<< Aff.killFiber (Aff.error "Cancelled")) (Map.values forks)
  runAff $ evalHaloM hs $ s.eval Finalize
