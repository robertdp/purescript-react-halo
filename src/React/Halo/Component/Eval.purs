module React.Halo.Eval where

import Prelude
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import React.Halo.Component (Lifecycle(..))
import React.Halo.Component.Control (HaloM)
import React.Halo.Component.State (StateRef)
import Unsafe.Coerce (unsafeCoerce)

type Interface props state action
  = { state :: StateRef props state action
    , send :: action -> Effect Unit
    , render :: state -> Effect Unit
    }

evalHaloM :: forall props state action. Interface props state action -> HaloM props state action Aff ~> Aff
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
  Props old new -> traverse_ eval.onAction $ eval.onProps old new
  Action action -> eval.onAction action
  Finalize -> traverse_ eval.onAction eval.finalize
