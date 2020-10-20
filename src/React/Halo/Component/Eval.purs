module React.Halo.Eval where

import Prelude
import Data.Either (either)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Effect.Aff as Aff
import Effect.Ref as Ref
import React.Halo.Component (Lifecycle(..))
import React.Halo.Component.Control (HaloM)
import React.Halo.Component.State (State(..))
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)

evalHaloM :: forall props state action. State props state action -> HaloM props state action Aff ~> Aff
evalHaloM = unsafeCoerce

type Spec props action state m
  = { initialize :: props -> Maybe action
    , onProps :: props -> props -> Maybe action
    , onAction :: action -> HaloM props state action m Unit
    , finalize :: Maybe action
    }

defaultEval :: forall props action state m. Spec props action state m
defaultEval =
  { initialize: \_ -> Nothing
  , onProps: \_ _ -> Nothing
  , onAction: \_ -> pure unit
  , finalize: Nothing
  }

makeEval :: forall props action state m. Spec props action state m -> Lifecycle props action -> HaloM props state action m Unit
makeEval eval = case _ of
  Initialize props -> traverse_ eval.onAction $ eval.initialize props
  Update old new -> traverse_ eval.onAction $ eval.onProps old new
  Action action -> eval.onAction action
  Finalize -> traverse_ eval.onAction eval.finalize

runAff :: Aff Unit -> Effect Unit
runAff = Aff.runAff_ (either throwError pure)

runInitialize :: forall props state action. State props action state -> props -> Effect Unit
runInitialize state@(State s) props = do
  Ref.write props s.props
  runAff $ evalHaloM state $ s.component.eval $ Initialize props

handleUpdate :: forall props state action. State props action state -> props -> Effect Unit
handleUpdate state@(State s) props = do
  props' <- Ref.read s.props
  unless (unsafeRefEq props props') do
    Ref.write props s.props
    runAff $ evalHaloM state $ s.component.eval $ Update props' props

handleAction :: forall props state action. State props state action -> action -> Effect Unit
handleAction state@(State s) action = do
  unlessM (Ref.read s.unmounted) do
    runAff $ evalHaloM state $ s.component.eval $ Action action

runFinalize :: forall props state action. State props state action -> Effect Unit
runFinalize state@(State s) = do
  Ref.write true s.unmounted
  runAff $ evalHaloM state $ s.component.eval Finalize
