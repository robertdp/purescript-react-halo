module React.Halo.Component
  ( ComponentSpec
  , component
  ) where

import Prelude

import Effect (Effect)
import Effect.Aff (Error)
import React.Basic.Hooks (Component, JSX)
import React.Basic.Hooks as React
import React.Halo.Handlers (Handlers)
import React.Halo.Hook (useHalo)
import React.Halo.Internal.Types (Activity, ErrorContext)

-- | Complete configuration for a Halo-owned React component.
-- |
-- | `initialState` receives the initial props once per mount. Later prop changes
-- | run `handlers.onPropsChange` and do not recreate state.
type ComponentSpec props state action key =
  { handlers :: Handlers props state action key
  , initialState :: props -> state
  , onError :: ErrorContext props action key -> Error -> Effect Unit
  , render ::
      { activity :: Activity key
      , dispatch :: action -> Effect Unit
      , props :: props
      , state :: state
      }
      -> JSX
  }

-- | Build a complete React component around a Halo action and task runtime.
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
      { handlers: spec.handlers
      , initialState
      , onError: spec.onError
      , props
      }
    pure $ spec.render
      { activity: halo.activity
      , dispatch: halo.dispatch
      , props
      , state: halo.state
      }
