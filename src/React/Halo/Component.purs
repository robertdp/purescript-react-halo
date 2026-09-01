module React.Halo.Component
  ( ComponentSpec
  , component
  ) where

import Prelude

import Effect (Effect)
import Effect.Aff (Error)
import React.Basic.Hooks (Component, JSX)
import React.Basic.Hooks as React
import React.Halo.Hook (useHalo)
import React.Halo.Internal.Runtime (HaloM)
import React.Halo.Internal.Types (Activity, ErrorContext, Lifecycle, TaskPolicy)

type ComponentSpec props state action key =
  { eval :: Lifecycle props action -> HaloM props state action key Unit
  , initialState :: props -> state
  , onError :: ErrorContext props action -> Error -> Effect Unit
  , render ::
      { activity :: Activity key
      , dispatch :: action -> Effect Unit
      , props :: props
      , state :: state
      }
      -> JSX
  , schedule :: action -> TaskPolicy key
  }

-- | Build a complete React component around a Halo action runtime.
component
  :: forall props state action key
   . Ord key
  => String
  -> ComponentSpec props state action key
  -> Component props
component name spec =
  React.component name \props -> React.do
    initialState <- React.useMemo unit \_ -> spec.initialState props
    halo <- useHalo
      { eval: spec.eval
      , initialState
      , onError: spec.onError
      , props
      , schedule: spec.schedule
      }
    pure $ spec.render
      { activity: halo.activity
      , dispatch: halo.dispatch
      , props
      , state: halo.state
      }
