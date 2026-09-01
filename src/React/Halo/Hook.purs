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
import React.Halo.Internal.Types (ErrorContext)

-- | Configuration for `useHalo`.
type HookSpec props state action m =
  { handlers :: Handlers props state action m
  , initialState :: state
  , onError :: ErrorContext props action -> Error -> Effect Unit
  , props :: props
  }

-- | State and action dispatch exposed to rendering code.
type HaloResult state action =
  { dispatch :: action -> Effect Unit
  , state :: state
  }

newtype UseHalo props state action m hooks = UseHalo
  ( UseEffect Unit
      ( UseEffect Unit
          ( UseEffect Unit
              ( UseMemo Unit (Runtime props state action m)
                  (UseState state hooks)
              )
          )
      )
  )

derive instance newtypeUseHalo :: Newtype (UseHalo props state action m hooks) _

-- | Run Halo inside a `react-basic-hooks` component.
-- |
-- | The natural transformation interprets application effects in `m` into the
-- | `Aff` fibers owned by the active React scope. New roots use the latest
-- | interpreter; roots already running retain their starting snapshot.
useHalo
  :: forall props state action m
   . (m ~> Aff)
  -> HookSpec props state action m
  -> Hook (UseHalo props state action m) (HaloResult state action)
useHalo runInAff { props, initialState, handlers, onError } =
  React.coerceHook React.do
    state /\ setState <- React.useState' initialState
    runtime <- React.useMemo unit \_ -> unsafePerformEffect $
      createRuntime runInAff
        { initialProps: props
        , initialState
        , spec: { handlers, onError }
        , stateUpdate: setState
        }
    React.useEffectAlways do
      syncSpec runtime runInAff
        { spec: { handlers, onError }
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
      { dispatch: dispatch runtime
      , state
      }
