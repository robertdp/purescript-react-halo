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

type Spec props state action m
  = { init :: state
    , eval :: Lifecycle props action -> HaloM props state action m Unit
    , render ::
        { props :: props
        , state :: state
        , dispatch :: action -> Effect Unit
        } ->
        JSX
    }
