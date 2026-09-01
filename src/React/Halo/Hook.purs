module React.Halo.Hook
  ( HaloHook
  , HookSpec
  , UseHalo(..)
  , useHalo
  ) where

import Prelude

import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Error)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (Hook, UseEffect, UseMemo, UseState)
import React.Basic.Hooks as React
import React.Halo.Internal.Runtime (HaloM, Runtime, activate, createRuntime, deactivate, dispatch, syncSpec, updateProps)
import React.Halo.Internal.Types (Activity, ErrorContext, Lifecycle, TaskPolicy, emptyActivity)

-- | Configuration for `useHalo`. The task key is chosen by the application and
-- | only needs an `Ord` instance.
type HookSpec props state action key =
  { eval :: Lifecycle props action -> HaloM props state action key Unit
  , initialState :: state
  , onError :: ErrorContext props action -> Error -> Effect Unit
  , props :: props
  , schedule :: action -> TaskPolicy key
  }

-- | Values exposed to rendering code.
type HaloHook state action key =
  { activity :: Activity key
  , dispatch :: action -> Effect Unit
  , state :: state
  }

newtype UseHalo props state action key hooks = UseHalo
  ( UseEffect Unit
      ( UseEffect Unit
          ( UseEffect Unit
              ( UseMemo Unit (Runtime props state action key)
                  ( UseState (Activity key)
                      (UseState state hooks)
                  )
              )
          )
      )
  )

derive instance newtypeUseHalo :: Newtype (UseHalo props state action key hooks) _

-- | Run Halo inside a `react-basic-hooks` component.
useHalo
  :: forall props state action key
   . Ord key
  => HookSpec props state action key
  -> Hook (UseHalo props state action key) (HaloHook state action key)
useHalo { props, initialState, eval, schedule, onError } =
  React.coerceHook React.do
    state /\ setState <- React.useState' initialState
    activity /\ setActivity <- React.useState' emptyActivity
    runtime <- React.useMemo unit \_ -> unsafePerformEffect $
      createRuntime
        { activityUpdate: setActivity
        , initialProps: props
        , initialState
        , spec: { eval, schedule, onError }
        , stateUpdate: setState
        }
    React.useEffectAlways do
      syncSpec runtime
        { activityUpdate: setActivity
        , spec: { eval, schedule, onError }
        , stateUpdate: setState
        }
      pure mempty
    React.useEffectOnce do
      activate runtime
      pure (deactivate runtime)
    React.useEffectAlways do
      updateProps runtime props
      pure mempty
    pure
      { activity
      , dispatch: dispatch runtime
      , state
      }
