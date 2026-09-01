module Test.Halo.GuideExamples where

import Prelude

import Control.Monad.State (modify_)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parallel, sequential)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import React.Halo as Halo

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
