module Test.Halo.DocExamples where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (modify_)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
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

type State = { greeting :: String }

initialState :: State
initialState = { greeting: "Not loaded" }

data Action = LoadGreeting

type UI a = Halo.HaloM Props State Action AppM a

handlers :: Halo.Handlers Props State Action AppM
handlers = Halo.defaultHandlers
  { onAction = case _ of
      LoadGreeting -> do
        greeting <- lift loadGreeting
        modify_ _ { greeting = greeting }
  }

loadButton :: Env -> Component Props
loadButton env = Halo.component "LoadButton" (runAppM env)
  { initialState: \_ -> initialState
  , handlers
  , onError: \_ error ->
      Console.error $ message error
  , render: \{ props, state, dispatch } ->
      R.div_
        [ R.text props.title
        , R.button
            { onClick: capture_ (dispatch LoadGreeting)
            , children: [ R.text "Load" ]
            }
        , R.text state.greeting
        ]
  }

useExample
  :: Env
  -> Props
  -> Hook (Halo.UseHalo Props State Action AppM) (Halo.HaloResult State Action)
useExample env props = Halo.useHalo (runAppM env)
  { props
  , initialState
  , handlers
  , onError: \_ _ -> pure unit
  }

type SearchState = { search :: Task.State String String }

searchSlot :: Task.Slot "search" SearchState String String
searchSlot = Task.slot (Proxy :: Proxy "search")

data SearchAction = Search String

searchHandler
  :: SearchAction
  -> Halo.HaloM Unit SearchState SearchAction Aff Unit
searchHandler = case _ of
  Search query -> Task.supersede searchSlot do
    pure (Right query)

renderSearch :: Task.View SearchState -> String
renderSearch tasks = case Task.toStatus tasks searchSlot of
  Task.Idle -> "Search"
  Task.Active -> "Searching"
  Task.Failed error -> error
  Task.Succeeded result -> result
