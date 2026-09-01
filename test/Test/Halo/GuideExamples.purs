module Test.Halo.GuideExamples where

import Prelude

import Control.Monad.State (gets, modify_)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parallel, sequential)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Lens (Lens', preview)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Exception as Exception
import React.Halo as Halo
import React.Halo.Task as Task
import Type.Proxy (Proxy(..))

type DashboardState =
  { feed :: Int
  , profile :: Int
  }

parallelExample :: Halo.HaloM Unit DashboardState Unit Aff Unit
parallelExample = do
  Tuple profile feed <- sequential ado
    profile <- parallel $ lift (pure 1 :: Aff Int)
    feed <- parallel $ lift (pure 2 :: Aff Int)
    in Tuple profile feed

  modify_ _ { profile = profile, feed = feed }

data FormAction
  = NameChanged String
  | Save

type FormState =
  { name :: String
  , savedName :: Maybe String
  }

formHandler :: FormAction -> Halo.HaloM Unit FormState FormAction Aff Unit
formHandler = case _ of
  NameChanged name -> modify_ _ { name = name }
  Save -> do
    name <- gets _.name
    saved <- lift (pure name :: Aff String)
    modify_ _ { savedName = Just saved }

propsHandler
  :: { value :: Int }
  -> Halo.HaloM { value :: Int } Unit Unit Aff Unit
propsHandler previous = do
  current <- Halo.getProps
  void $ lift (pure $ previous.value + current.value :: Aff Int)

type SearchState =
  { query :: String
  , search :: Task.State String Int
  }

searchSlot :: Task.Slot "search" SearchState String Int
searchSlot = Task.slot (Proxy :: Proxy "search")

initialSearchState :: SearchState
initialSearchState =
  { query: ""
  , search: Task.idle searchSlot
  }

search :: String -> Halo.HaloM Unit SearchState Unit Aff (Either String Int)
search query = do
  modify_ _ { query = query }
  lift (pure (Right 1) :: Aff (Either String Int))

taskPolicies :: Halo.HaloM Unit SearchState Unit Aff Unit
taskPolicies = do
  Task.once searchSlot (search "initial")
  Task.startIfInactive searchSlot (search "save")
  Task.supersede searchSlot (search "latest")
  Task.debounce searchSlot (Milliseconds 250.0) (search "debounced")
  Task.reset searchSlot

renderSearch :: Task.View SearchState -> String
renderSearch tasks = case Task.toStatus tasks searchSlot of
  Task.Idle -> "Search"
  Task.Active -> "Searching"
  Task.Failed error -> error
  Task.Succeeded _ -> "Done"

successfulSearch :: Task.View SearchState -> Maybe Int
successfulSearch tasks = preview Task._Succeeded (Task.toStatus tasks searchSlot)

type NestedState =
  { tasks :: { search :: Task.State String Int }
  }

tasksLens :: Lens' NestedState { search :: Task.State String Int }
tasksLens = prop (Proxy :: Proxy "tasks")

nestedSearchLens :: Lens' { search :: Task.State String Int } (Task.State String Int)
nestedSearchLens = prop (Proxy :: Proxy "search")

nestedSearchSlot :: Task.Slot "nestedSearch" NestedState String Int
nestedSearchSlot = Task.slotAt (Proxy :: Proxy "nestedSearch") (tasksLens <<< nestedSearchLens)

type ProcessState = { synchronization :: Maybe Halo.ForkId }

startSynchronization :: Halo.HaloM Unit ProcessState Unit Aff Unit
startSynchronization = do
  fiber <- Halo.fork (lift (pure unit :: Aff Unit))
  modify_ _ { synchronization = Just fiber }

cancelSynchronization :: Halo.HaloM Unit ProcessState Unit Aff Unit
cancelSynchronization = do
  current <- gets _.synchronization
  traverse_ Halo.kill current
  modify_ _ { synchronization = Nothing }

data ExternalAction = NameReceived String

names :: Halo.Emitter String
names = Halo.makeEmitter \emit -> do
  emit "Halo"
  pure (pure unit)

actions :: Halo.Emitter ExternalAction
actions = NameReceived <$> names

subscriptionExample :: Halo.HaloM Unit Unit ExternalAction Aff Unit
subscriptionExample = do
  subscriptionId <- Halo.subscribeWithId \_ -> actions
  Halo.unsubscribe subscriptionId

cleanupExample
  :: Effect Unit
  -> Halo.HaloM Unit Unit ExternalAction Aff Unit
cleanupExample removeListener = do
  cleanupId <- Halo.registerCleanup removeListener
  Halo.releaseCleanup cleanupId

lifecycleHandlers :: Halo.Handlers Unit Unit ExternalAction Aff
lifecycleHandlers = Halo.defaultHandlers
  { onActivate = void $ Halo.subscribe actions
  , onPropsChange = \_ -> void Halo.getProps
  , onAction = \_ -> pure unit
  }

errorExample
  :: Halo.ErrorContext Unit ExternalAction
  -> Exception.Error
  -> Effect Unit
errorExample context _ = case context of
  Halo.ActivationError -> pure unit
  Halo.PropsChangeError _ -> pure unit
  Halo.ActionError _ -> pure unit
  Halo.ForkError _ -> pure unit
  Halo.DeactivationError -> pure unit
