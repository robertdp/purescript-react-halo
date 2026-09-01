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
import React.Halo.Internal.Task.Types (View)
import React.Halo.Internal.Types (ErrorContext)

-- | Configuration for a complete Halo-owned React component.
-- |
-- | `initialState` receives the initial props once per mount. Later prop changes
-- | start `handlers.onPropsChange` and do not recreate state. The renderer
-- | receives current props plus one coherent state/task-view snapshot and
-- | synchronous action dispatch.
type ComponentSpec props state action m =
  { handlers :: Handlers props state action m
  , initialState :: props -> state
  , onError :: ErrorContext props action -> Error -> Effect Unit
  , render ::
      { dispatch :: action -> Effect Unit
      , props :: props
      , state :: state
      , tasks :: View state
      }
      -> JSX
  }

-- | Build a complete React component around a Halo action runtime.
-- |
-- | The natural transformation is the application boundary: it translates `m`
-- | into the component-owned `Aff` roots managed by Halo. It must return the
-- | computation that performs the work rather than detach it.
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
      , tasks: halo.tasks
      }
