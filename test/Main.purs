module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import React.Halo (Lifecycle(..), modify_)
import React.Halo.Internal.Eval as Eval
import React.Halo.Internal.State as State
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_ do
    runSpec [ consoleReporter ] do
      describe "purescript-react-halo" do
        describe "Props" runPropsTests
        describe "State" runStateTests
        describe "Subscriptions" runSubscriptionTests
        describe "Parallelism" runParallelismTests
        describe "Forking" runForkingTests

runPropsTests :: Spec Unit
runPropsTests = do
  describe "Update" do
    it "does not fire in initialization" do
      { expect } <- makeUpdateState
      expect 0
    it "does not fire when props are referentially equal" do
      { state, initialProps, expect } <- makeUpdateState
      liftEffect $ Eval.handleUpdate state initialProps
      expect 0
    it "does fire when props are not referentially equal" do
      { state, expect } <- makeUpdateState
      liftEffect $ Eval.handleUpdate state { value: "new object" }
      expect 1
  where
  makeUpdateState =
    liftEffect do
      count <- Ref.new 0
      let
        eval = case _ of
          Update _ _ -> liftEffect $ Ref.modify_ (add 1) count
          _ -> pure unit

        initialProps = { value: "" }

        expect x = liftEffect (Ref.read count) >>= shouldEqual x
      state <- State.createInitialState { props: initialProps, initialState: unit, eval, update: mempty }
      Eval.runInitialize state
      pure { state, initialProps, expect }

runStateTests :: Spec Unit
runStateTests = do
  it "correctly modifies the state" do
    { modify, expect, read } <- makeState { value: "" }
    modify \s -> s { value = "first" }
    modify \s -> s { value = s.value <> " test" }
    value <- read
    value `shouldEqual` { value: "first test" }
    expect 2
  it "does not modify the state when the reference does not change" do
    { modify, expect, read } <- makeState { value: "" }
    modify identity
    value <- read
    value `shouldEqual` { value: "" }
    expect 0
  where
  makeState initialState =
    liftEffect do
      count <- Ref.new 0
      value <- Ref.new initialState
      let
        update state = do
          Ref.write state value
          Ref.modify_ (add 1) count

        read = liftEffect $ Ref.read value

        expect x = liftEffect (Ref.read count) >>= shouldEqual x

        eval = case _ of
          Action f -> modify_ f
          _ -> pure unit
      state <- State.createInitialState { props: unit, initialState, eval, update }
      Eval.runInitialize state
      let
        modify = liftEffect <<< Eval.handleAction state
      pure { expect, modify, state, read }

runSubscriptionTests :: Spec Unit
runSubscriptionTests = pure unit

runParallelismTests :: Spec Unit
runParallelismTests = pure unit

runForkingTests :: Spec Unit
runForkingTests = pure unit
