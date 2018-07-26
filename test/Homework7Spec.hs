module Homework7Spec where

import qualified Test.Hspec as Hspec

import qualified Homework7.Homework7 as HW

spec :: Hspec.Spec
spec = Hspec.describe "Homework7" $
  Hspec.it "ex1:" $
    1 `Hspec.shouldBe` 1
