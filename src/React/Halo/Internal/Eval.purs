module React.Halo.Internal.Eval
  ( EvalSpec
  , defaultEval
  , mkEval
  ) where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import React.Halo.Internal.Runtime (HaloM)
import React.Halo.Internal.Types (Lifecycle(..))

-- | Convenience configuration for routing lifecycle events into the same action
-- | handler used by dispatched actions.
type EvalSpec props state action key =
  { handleAction :: action -> HaloM props state action key Unit
  , initialize :: Maybe action
  , update :: props -> Maybe action
  }

-- | An evaluator that ignores activation and prop updates until configured.
defaultEval :: forall props state action key. EvalSpec props state action key
defaultEval =
  { handleAction: \_ -> pure unit
  , initialize: Nothing
  , update: \_ -> Nothing
  }

mkEval
  :: forall props state action key
   . EvalSpec props state action key
  -> Lifecycle props action
  -> HaloM props state action key Unit
mkEval eval = case _ of
  Activate -> traverse_ eval.handleAction eval.initialize
  Update previousProps -> traverse_ eval.handleAction (eval.update previousProps)
  Action action -> eval.handleAction action
