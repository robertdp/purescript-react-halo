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

data TaskKey = GreetingRequest

derive instance eqTaskKey :: Eq TaskKey
derive instance ordTaskKey :: Ord TaskKey

loadGreetingTask :: Halo.Task Props State Action TaskKey Unit
loadGreetingTask = Halo.restartable GreetingRequest \_ -> do
  modify_ _ { loading = true, result = Nothing }
  Props { loadGreeting } <- Halo.props
  outcome <- liftAff $ attempt loadGreeting
  modify_ _
    { loading = false
    , result = Just $ case outcome of
        Left error -> Left (message error)
        Right greeting -> Right greeting
    }

loadButton :: Component Props
loadButton = Halo.component "LoadButton"
  { initialState: \_ -> { loading: false, result: Nothing }
  , handlers: Halo.defaultHandlers
      { onAction = \Load -> Halo.perform_ loadGreetingTask }
  , onError: \context error ->
      Console.error $ "Unexpected Halo failure in " <> showContext context <> ": " <> message error
  , render: \{ state, dispatch, activity } ->
      let
        counts = Halo.activity loadGreetingTask activity
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

showContext :: Halo.ErrorContext Props Action TaskKey -> String
showContext = case _ of
  Halo.ActivationError -> "activation"
  Halo.DeactivationError -> "deactivation"
  Halo.PropsChangeError _ -> "props change"
  Halo.ActionError Load -> "Load action"
  Halo.TaskError GreetingRequest -> "greeting task"
  Halo.TaskConfigurationError GreetingRequest -> "greeting task definition"

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
  | Metrics

derive instance eqWorkflowTask :: Eq WorkflowTask
derive instance ordWorkflowTask :: Ord WorkflowTask

searchTask :: Halo.Task Unit Unit WorkflowAction WorkflowTask String
searchTask = Halo.restartable SearchRequest \_ -> pure unit

saveTask :: Halo.Task Unit Unit WorkflowAction WorkflowTask Unit
saveTask = Halo.drop SaveRequest \_ -> pure unit

autosaveTask :: Halo.Task Unit Unit WorkflowAction WorkflowTask String
autosaveTask = Halo.keepLatest AutosaveRequest \_ -> pure unit

uploadTask :: Int -> Halo.Task Unit Unit WorkflowAction WorkflowTask Int
uploadTask fileId = Halo.enqueue (Upload fileId) \_ -> pure unit

metricTask :: Halo.Task Unit Unit WorkflowAction WorkflowTask String
metricTask = Halo.concurrent Metrics \_ -> pure unit

handleWorkflow
  :: WorkflowAction
  -> Halo.HaloM Unit Unit WorkflowAction WorkflowTask Unit
handleWorkflow = case _ of
  SearchChanged query -> Halo.perform searchTask query
  SaveClicked -> Halo.perform_ saveTask
  Autosave draft -> Halo.perform autosaveTask draft
  UploadChunk fileId chunk -> Halo.perform (uploadTask fileId) chunk
  RecordMetric name -> Halo.perform metricTask name

data SimpleAction = InitializeData

simpleEmitter :: Halo.Emitter SimpleAction
simpleEmitter = Halo.makeEmitter \_ -> pure (pure unit)

simpleHandlers :: Halo.Handlers Unit Unit SimpleAction Unit
simpleHandlers = Halo.defaultHandlers
  { onActivate = pure unit
  , onAction = \InitializeData -> pure unit
  }
