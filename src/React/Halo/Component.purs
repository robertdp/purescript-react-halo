module React.Halo.Component where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import React.Basic.Hooks (Component, JSX)
import React.Basic.Hooks as React
import React.Halo.Hook (useHalo)
import React.Halo.Internal.Control (HaloM)
import React.Halo.Internal.Types (Lifecycle)

type ComponentSpec props state action m =
  { initialState :: props -> state
  , eval :: Lifecycle props action -> HaloM props state action m Unit
  , render ::
      { props :: props
      , state :: state
      , send :: action -> Effect Unit
      }
      -> JSX
  }

-- | Build a component by providing a name and a Halo component spec.
component
  :: forall props state action
   . String
  -> ComponentSpec props state action Aff
  -> Component props
component name spec@{ eval, render } =
  React.component name \props -> React.do
    initialState <- React.useMemo unit \_ -> spec.initialState props
    state /\ send <- useHalo { props, initialState, eval }
    pure (render { props, state, send })
