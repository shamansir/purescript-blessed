module Test.Main where

import Prelude (Unit, ($), discard)

import Effect (Effect)
import Effect.Aff (launchAff_)

import Test.Spec (describe, describeOnly, pending')
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Test.NodeKey (spec) as NodeKey


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Node Key"
    NodeKey.spec
