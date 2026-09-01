module React.Halo.Hook
  ( HaloResult
  , HookSpec
  , UseHalo(..)
  , useHalo
  ) where

import Prelude

import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, Error)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (Hook, UseEffect, UseMemo, UseState)
import React.Basic.Hooks as React
import React.Halo.Handlers (Handlers)
import React.Halo.Internal.Runtime (Runtime, activate, createRuntime, deactivate, dispatch, syncSpec, updateProps)
import React.Halo.Internal.Task.Types (View)
import React.Halo.Internal.Task.Types as Task
import React.Halo.Internal.Types (ErrorContext)

-- | Configuration for `useHalo`.
-- |
-- | `initialState` initializes the hook once. Later `props` changes start
-- | `handlers.onPropsChange` without recreating state. New roots use the latest
-- | handlers, error callback, state setter, and application interpreter supplied
-- | by a render.
type HookSpec props state action m =
  { handlers :: Handlers props state action m
  , initialState :: state
  , onError :: ErrorContext props action -> Error -> Effect Unit
  , props :: props
  }

-- | Coherent component state, immutable task-authority view, and synchronous
-- | action dispatch exposed to rendering code. Dispatch starts an independent
-- | handler root while the current React activation is active.
type HaloResult state action =
  { dispatch :: action -> Effect Unit
  , state :: state
  , tasks :: View state
  }

newtype UseHalo props state action m hooks = UseHalo
  ( UseEffect Unit
      ( UseEffect Unit
          ( UseEffect Unit
              ( UseMemo Unit (Runtime props state action m)
                  (UseState { state :: state, tasks :: View state } hooks)
              )
          )
      )
  )

derive instance newtypeUseHalo :: Newtype (UseHalo props state action m hooks) _

-- | Run Halo inside a `react-basic-hooks` component.
-- |
-- | The natural transformation interprets application effects in `m` inside
-- | `Aff` roots owned by the active React scope. It must return the computation
-- | that performs the work rather than detach it. New handlers use the latest
-- | interpreter; existing roots retain their snapshot, and a fork inherits the
-- | snapshot of the root that launches it.
-- |
-- | Effect cleanup fences the activation, normalizes managed task state, runs
-- | synchronous cleanup, and requests cancellation of every handler and fork.
-- | A StrictMode setup replay publishes normalized state before new work.
useHalo
  :: forall props state action m
   . (m ~> Aff)
  -> HookSpec props state action m
  -> Hook (UseHalo props state action m) (HaloResult state action)
useHalo runInAff { props, initialState, handlers, onError } =
  React.coerceHook React.do
    snapshot /\ setSnapshot <- React.useState'
      { state: initialState
      , tasks: Task.emptyView initialState
      }
    runtime <- React.useMemo unit \_ -> unsafePerformEffect $
      createRuntime runInAff
        { initialProps: props
        , initialState
        , spec: { handlers, onError }
        , stateUpdate: \state tasks -> setSnapshot { state, tasks }
        }
    React.useEffectAlways do
      syncSpec runtime runInAff
        { spec: { handlers, onError }
        , stateUpdate: \state tasks -> setSnapshot { state, tasks }
        }
      pure mempty
    React.useEffectOnce do
      activate runtime
      pure (deactivate runtime)
    React.useEffectAlways do
      updateProps runtime props
      pure mempty
    pure
      { dispatch: dispatch runtime
      , state: snapshot.state
      , tasks: snapshot.tasks
      }
