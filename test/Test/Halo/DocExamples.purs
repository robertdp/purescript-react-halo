module Test.Halo.DocExamples where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (gets, modify_)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console
import Effect.Exception (message)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (Component, Hook)
import React.Halo as Halo

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
  { fiber :: Maybe Halo.ForkId
  , loading :: Boolean
  , result :: Maybe String
  }

data Action
  = Load
  | Cancel

type UI a = Halo.HaloM Props State Action AppM a

handlers :: Halo.Handlers Props State Action AppM
handlers = Halo.defaultHandlers
  { onAction = case _ of
      Load -> do
        previous <- gets _.fiber
        traverse_ Halo.kill previous
        fiber <- Halo.fork do
          modify_ _ { loading = true, result = Nothing }
          result <- lift loadGreeting
          modify_ _ { loading = false, result = Just result }
        modify_ _ { fiber = Just fiber }
      Cancel -> do
        previous <- gets _.fiber
        traverse_ Halo.kill previous
        modify_ _ { fiber = Nothing, loading = false }
  }

loadButton :: Env -> Component Props
loadButton env = Halo.component "LoadButton" (runAppM env)
  { initialState: \_ -> { fiber: Nothing, loading: false, result: Nothing }
  , handlers
  , onError: \_ error ->
      Console.error $ "Unexpected Halo error: " <> message error
  , render: \{ props, state, dispatch } ->
      R.div_
        [ R.text props.title
        , R.button
            { onClick: capture_ (dispatch Load)
            , children: [ R.text if state.loading then "Restart" else "Load" ]
            }
        , R.button
            { onClick: capture_ (dispatch Cancel)
            , children: [ R.text "Cancel" ]
            }
        , R.text $ case state.result of
            Nothing -> if state.loading then "Loading…" else "Not loaded"
            Just greeting -> greeting
        ]
  }

useExample
  :: Env
  -> Props
  -> Hook (Halo.UseHalo Props State Action AppM) (Halo.HaloResult State Action)
useExample env props = Halo.useHalo (runAppM env)
  { props
  , initialState: { fiber: Nothing, loading: false, result: Nothing }
  , handlers
  , onError: \_ _ -> pure unit
  }
