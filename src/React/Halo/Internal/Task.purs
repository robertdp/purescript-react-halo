module React.Halo.Internal.Task
  ( Strategy(..)
  , Task(..)
  , key
  , run
  , strategy
  , strategyName
  ) where

import Prelude

-- | Runtime-only scheduling modes. Public code chooses one by constructing a
-- | first-class task definition.
data Strategy
  = Concurrent
  | Restartable
  | Drop
  | Enqueue
  | KeepLatest

derive instance eqStrategy :: Eq Strategy

-- | Internal representation parameterized by its computation monad.
data Task m key input = Task key Strategy (input -> m Unit)

key :: forall m key input. Task m key input -> key
key (Task taskKey _ _) = taskKey

strategy :: forall m key input. Task m key input -> Strategy
strategy (Task _ taskStrategy _) = taskStrategy

run :: forall m key input. Task m key input -> input -> m Unit
run (Task _ _ implementation) = implementation

strategyName :: Strategy -> String
strategyName = case _ of
  Concurrent -> "concurrent"
  Restartable -> "restartable"
  Drop -> "drop"
  Enqueue -> "enqueue"
  KeepLatest -> "keepLatest"
