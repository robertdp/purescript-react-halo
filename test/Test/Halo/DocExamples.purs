module Test.Halo.DocExamples where

import Prelude

import Control.Monad.State (modify_)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, attempt)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console as Console
import Effect.Exception (message)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (Component)
import React.Halo as Halo

newtype Props = Props { loadGreeting :: Aff String }

type State =
  { loading :: Boolean
  , result :: Maybe (Either String String)
  }

data Action = Load

data Task = GreetingRequest

derive instance eqTask :: Eq Task
derive instance ordTask :: Ord Task

loadButton :: Component Props
loadButton = Halo.component "LoadButton"
  { initialState: \_ -> { loading: false, result: Nothing }
  , handlers: Halo.defaultHandlers
      { onAction = \Load -> Halo.startTask (Halo.Restartable GreetingRequest) do
          modify_ _ { loading = true, result = Nothing }
          Props { loadGreeting } <- Halo.props
          outcome <- liftAff $ attempt loadGreeting
          modify_ _
            { loading = false
            , result = Just $ case outcome of
                Left error -> Left (message error)
                Right greeting -> Right greeting
            }
      }
  , onError: \context error ->
      Console.error $ "Unexpected Halo failure in " <> showContext context <> ": " <> message error
  , render: \{ state, dispatch, activity } ->
      let
        counts = Halo.activityFor GreetingRequest activity
      in
        R.div_
          [ R.button
              { onClick: capture_ (dispatch Load)
              , children: [ R.text if counts.running > 0 then "Restart load" else "Load" ]
              }
          , R.text $ case state.result of
              Nothing -> if state.loading then "Loading…" else "Not loaded"
              Just (Left error) -> error
              Just (Right greeting) -> greeting
          ]
  }

showContext :: Halo.ErrorContext Props Action Task -> String
showContext = case _ of
  Halo.ActivationError -> "activation"
  Halo.DeactivationError -> "deactivation"
  Halo.PropsChangeError _ -> "props change"
  Halo.ActionError Load -> "Load action"
  Halo.TaskError _ -> "greeting task"

data WorkflowAction
  = SearchChanged String
  | SaveClicked
  | Autosave String
  | UploadChunk Int Int
  | RecordMetric String

data WorkflowTask
  = SearchRequest
  | SaveRequest
  | AutosaveRequest
  | Upload Int

derive instance eqWorkflowTask :: Eq WorkflowTask
derive instance ordWorkflowTask :: Ord WorkflowTask

handleWorkflow
  :: WorkflowAction
  -> Halo.HaloM Unit Unit WorkflowAction WorkflowTask Unit
handleWorkflow = case _ of
  SearchChanged _ -> Halo.startTask (Halo.Restartable SearchRequest) (pure unit)
  SaveClicked -> Halo.startTask (Halo.Drop SaveRequest) (pure unit)
  Autosave _ -> Halo.startTask (Halo.KeepLatest AutosaveRequest) (pure unit)
  UploadChunk fileId _ -> Halo.startTask (Halo.Enqueue (Upload fileId)) (pure unit)
  RecordMetric _ -> Halo.startTask Halo.Every (pure unit)

data SimpleAction = InitializeData

simpleEmitter :: Halo.Emitter SimpleAction
simpleEmitter = Halo.makeEmitter \_ -> pure (pure unit)

simpleHandlers :: Halo.Handlers Unit Unit SimpleAction Unit
simpleHandlers = Halo.defaultHandlers
  { onActivate = pure unit
  , onAction = \InitializeData -> pure unit
  }
