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
import React.Halo.Handlers (Handlers)
import React.Halo.Internal.Runtime (Runtime, activate, createRuntime, deactivate, dispatch, syncSpec, updateProps)
import React.Halo.Internal.Types (Activity, ErrorContext, emptyActivity)

-- | Configuration for `useHalo`.
-- |
-- | The application chooses `key`; only explicit tasks use it, and it needs an
-- | `Ord` instance so Halo can coordinate keyed task slots.
type HookSpec props state action key =
  { handlers :: Handlers props state action key
  , initialState :: state
  , onError :: ErrorContext props action key -> Error -> Effect Unit
  , props :: props
  }

-- | State, action dispatch, and explicit task activity exposed to rendering code.
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
-- |
-- | React effect activation owns the runtime scope. Cleanup deactivates it, and
-- | a later StrictMode replay creates a fresh usable scope.
useHalo
  :: forall props state action key
   . Ord key
  => HookSpec props state action key
  -> Hook (UseHalo props state action key) (HaloHook state action key)
useHalo { props, initialState, handlers, onError } =
  React.coerceHook React.do
    state /\ setState <- React.useState' initialState
    activity /\ setActivity <- React.useState' emptyActivity
    runtime <- React.useMemo unit \_ -> unsafePerformEffect $
      createRuntime
        { activityUpdate: setActivity
        , initialProps: props
        , initialState
        , spec: { handlers, onError }
        , stateUpdate: setState
        }
    React.useEffectAlways do
      syncSpec runtime
        { activityUpdate: setActivity
        , spec: { handlers, onError }
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
