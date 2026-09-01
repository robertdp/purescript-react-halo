module Test.Halo.DocExamples where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console
import Effect.Exception (message)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (Component, Hook)
import React.Halo as Halo
import React.Halo.Task as Task
import Type.Proxy (Proxy(..))

type Env = { loadGreeting :: Aff String }

newtype AppM a = AppM (ReaderT Env Aff a)

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM computation) = runReaderT computation env

loadGreeting :: AppM String
loadGreeting = AppM do
  env <- ask
  liftAff env.loadGreeting

type Props = { title :: String }

type State =
  { greeting :: Task.State String String
  }

greetingLens :: Lens' State (Task.State String String)
greetingLens = prop (Proxy :: Proxy "greeting")

greetingSlot :: Task.Slot "greeting" State String String
greetingSlot = Task.slot (Proxy :: Proxy "greeting") greetingLens

data Action
  = Load
  | Cancel

type UI a = Halo.HaloM Props State Action AppM a

handlers :: Halo.Handlers Props State Action AppM
handlers = Halo.defaultHandlers
  { onAction = case _ of
      Load -> Task.supersede greetingSlot do
        greeting <- lift loadGreeting
        pure (Right greeting)
      Cancel -> Task.reset greetingSlot
  }

loadButton :: Env -> Component Props
loadButton env = Halo.component "LoadButton" (runAppM env)
  { initialState: \_ -> { greeting: Task.idle greetingSlot }
  , handlers
  , onError: \_ error ->
      Console.error $ "Unexpected Halo error: " <> message error
  , render: \{ props, tasks, dispatch } ->
      R.div_
        [ R.text props.title
        , R.button
            { onClick: capture_ (dispatch Load)
            , children: [ R.text if Task.isActive tasks greetingSlot then "Restart" else "Load" ]
            }
        , R.button
            { onClick: capture_ (dispatch Cancel)
            , children: [ R.text "Cancel" ]
            }
        , R.text $ case Task.toStatus tasks greetingSlot of
            Task.Idle -> "Not loaded"
            Task.Active -> "Loading…"
            Task.Failed error -> error
            Task.Succeeded greeting -> greeting
        ]
  }

useExample
  :: Env
  -> Props
  -> Hook (Halo.UseHalo Props State Action AppM) (Halo.HaloResult State Action)
useExample env props = Halo.useHalo (runAppM env)
  { props
  , initialState: { greeting: Task.idle greetingSlot }
  , handlers
  , onError: \_ _ -> pure unit
  }
