module Test.Halo.RuntimeSpec (spec) where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (modify_)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parallel, sequential)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.AVar (AVar)
import Effect.AVar as EffectAVar
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import React.Halo.Handlers (Handlers, defaultHandlers)
import React.Halo.Internal.Runtime (Runtime, activate, createRuntime, deactivate, dispatch, syncSpec)
import Test.Halo.Helpers (Gate, await, makeGate, release, waitForGate)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

newtype AppM a = AppM (ReaderT Int Aff a)

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

runAppM :: Int -> AppM ~> Aff
runAppM environment (AppM computation) = runReaderT computation environment

readEnvironment :: AppM Int
readEnvironment = AppM ask

data Action
  = ReadEnvironment Gate (AVar Int)
  | RunParallel Gate Gate (AVar Unit)

type State = Int

handlers :: Handlers Unit State Action AppM
handlers = defaultHandlers
  { onAction = case _ of
      ReadEnvironment gate completed -> do
        liftAff $ waitForGate gate
        environment <- lift readEnvironment
        liftAff $ void $ AVar.tryPut environment completed
      RunParallel left right completed -> do
        Tuple a b <- sequential ado
          a <- parallel do
            liftAff $ waitForGate left
            pure 1
          b <- parallel do
            liftAff $ waitForGate right
            pure 2
          in Tuple a b
        modify_ (\state -> state + a + b)
        liftAff $ void $ AVar.tryPut unit completed
  }

makeRuntime
  :: Int
  -> Aff
       { runtime :: Runtime Unit State Action AppM
       , state :: Ref.Ref State
       }
makeRuntime environment = liftEffect do
  state <- Ref.new 0
  runtime <- createRuntime (runAppM environment)
    { initialProps: unit
    , initialState: 0
    , spec: { handlers, onError: \_ _ -> pure unit }
    , stateUpdate: flip Ref.write state
    }
  activate runtime
  pure { runtime, state }

spec :: Spec Unit
spec = describe "application monad and parallelism" do
  it "lifts AppM and snapshots the interpreter for each root" do
    { runtime, state } <- makeRuntime 1
    Aff.finally (liftEffect $ deactivate runtime) do
      firstGate <- liftEffect makeGate
      firstResult <- liftEffect EffectAVar.empty
      liftEffect $ dispatch runtime (ReadEnvironment firstGate firstResult)
      void $ await "first root start" firstGate.started

      liftEffect $ syncSpec runtime (runAppM 2)
        { spec: { handlers, onError: \_ _ -> pure unit }
        , stateUpdate: flip Ref.write state
        }

      secondGate <- liftEffect makeGate
      secondResult <- liftEffect EffectAVar.empty
      liftEffect $ dispatch runtime (ReadEnvironment secondGate secondResult)
      void $ await "second root start" secondGate.started

      release firstGate
      release secondGate
      first <- await "first environment" firstResult
      second <- await "second environment" secondResult
      first `shouldEqual` 1
      second `shouldEqual` 2

  it "runs Halo branches concurrently with the direct Parallel instance" do
    { runtime, state } <- makeRuntime 1
    Aff.finally (liftEffect $ deactivate runtime) do
      left <- liftEffect makeGate
      right <- liftEffect makeGate
      completed <- liftEffect EffectAVar.empty

      liftEffect $ dispatch runtime (RunParallel left right completed)
      void $ await "left parallel branch" left.started
      void $ await "right parallel branch" right.started

      release left
      release right
      void $ await "parallel handler completion" completed
      value <- liftEffect $ Ref.read state
      value `shouldEqual` 3
