module Test.Main where

import Prelude

import Effect (Effect)
import Test.Halo.ScopeHandlerSpec as ScopeHandlerSpec
import Test.Halo.SchedulerSpec as SchedulerSpec
import Test.Halo.SubscriptionErrorSpec as SubscriptionErrorSpec
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  SchedulerSpec.spec
  ScopeHandlerSpec.spec
  SubscriptionErrorSpec.spec
