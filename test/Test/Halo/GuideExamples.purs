module Test.Halo.GuideExamples where

import Prelude

import Control.Monad.State (modify_)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parallel, sequential)
import Data.Either (Either(..))
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Milliseconds(..))
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

data SimpleAction = InitializeData

simpleEmitter :: Halo.Emitter SimpleAction
simpleEmitter = Halo.makeEmitter \_ -> pure (pure unit)

simpleSubscription :: Halo.HaloM Unit Unit SimpleAction Aff Unit
simpleSubscription = void $ Halo.subscribeWithId \_ -> simpleEmitter

genericCleanup :: Halo.HaloM Unit Unit SimpleAction Aff Unit
genericCleanup = do
  cleanupId <- Halo.registerCleanup (pure unit)
  Halo.releaseCleanup cleanupId

type SearchState =
  { query :: String
  , search :: Task.State String Int
  }

data SearchAction = Search String | CancelSearch

searchLens :: Lens' SearchState (Task.State String Int)
searchLens = prop (Proxy :: Proxy "search")

retryingSearch :: String -> Aff (Either String Int)
retryingSearch _ = pure (Right 1)

searchHandler
  :: SearchAction
  -> Halo.HaloM Unit SearchState SearchAction Aff Unit
searchHandler = case _ of
  Search query -> Task.debounce searchLens (Milliseconds 250.0) do
    modify_ _ { query = query }
    lift (retryingSearch query)
  CancelSearch -> Task.reset searchLens

renderSearch :: SearchState -> String
renderSearch state = case Task.toStatus state.search of
  Task.Idle -> "Search"
  Task.Active -> "Searching"
  Task.Failed error -> error
  Task.Succeeded _ -> "Done"
