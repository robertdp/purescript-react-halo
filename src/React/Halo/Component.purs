module React.Halo.Component where

import Prelude
import Effect (Effect)
import React.Basic (JSX)
import React.Halo.Component.Control (HaloM)

data Lifecycle props action
  = Initialize props
  | Update props props
  | Action action
  | Finalize

type ComponentSpec props state action m
  = { initialState :: state
    , eval :: Lifecycle props action -> HaloM props state action m Unit
    , render ::
        { props :: props
        , state :: state
        , send :: action -> Effect Unit
        } ->
        JSX
    }

type HookSpec props state action m
  = { props :: props
    , initialState :: state
    , eval :: Lifecycle props action -> HaloM props state action m Unit
    }
