module React.Halo.Component
  ( ComponentSpec
  , component
  ) where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, Error)
import React.Basic.Hooks (Component, JSX)
import React.Basic.Hooks as React
import React.Halo.Handlers (Handlers)
import React.Halo.Hook (useHalo)
import React.Halo.Internal.Types (ErrorContext)

-- | Complete configuration for a Halo-owned React component.
-- |
-- | `initialState` receives the initial props once per mount. Later prop changes
-- | run `handlers.onPropsChange` and do not recreate state.
type ComponentSpec props state action m =
  { handlers :: Handlers props state action m
  , initialState :: props -> state
  , onError :: ErrorContext props action -> Error -> Effect Unit
  , render ::
      { dispatch :: action -> Effect Unit
      , props :: props
      , state :: state
      }
      -> JSX
  }

-- | Build a complete React component around a Halo action runtime.
-- |
-- | The natural transformation is the application boundary: it translates the
-- | component's application monad into the `Aff` fibers owned by Halo.
component
  :: forall props state action m
   . String
  -> (m ~> Aff)
  -> ComponentSpec props state action m
  -> Component props
component name runInAff spec =
  React.component name \props -> React.do
    initialState <- React.useMemo unit \_ -> spec.initialState props
    halo <- useHalo runInAff
      { handlers: spec.handlers
      , initialState
      , onError: spec.onError
      , props
      }
    pure $ spec.render
      { dispatch: halo.dispatch
      , props
      , state: halo.state
      }
