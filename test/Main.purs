module Test.Main where

import Prelude

import Effect (Effect)
import Test.Halo.ScopeHandlerSpec as ScopeHandlerSpec
import Test.Halo.RuntimeSpec as RuntimeSpec
import Test.Halo.SubscriptionErrorSpec as SubscriptionErrorSpec
import Test.Halo.TaskSpec as TaskSpec
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  RuntimeSpec.spec
  ScopeHandlerSpec.spec
  SubscriptionErrorSpec.spec
  TaskSpec.spec
