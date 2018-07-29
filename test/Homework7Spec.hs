module Homework7Spec where

import qualified Data.Monoid as Monoid
import qualified Test.Hspec as Hspec

import qualified Homework7.Homework7 as HW

spec :: Hspec.Spec
spec = Hspec.describe "Homework7" $ do
  Hspec.it "ex1: tag" $ do
    HW.tag (HW.Single (Monoid.Product 3) HW.Empty)
      `Hspec.shouldBe` 3

    HW.tag (HW.Append (Monoid.Product 5) HW.Empty HW.Empty)
      `Hspec.shouldBe` 5

  Hspec.it "ex1: (+++)" $
    HW.tag (HW.Single (Monoid.Product 3) HW.Empty
      HW.+++
      HW.Append (Monoid.Product 5) HW.Empty HW.Empty)
      `Hspec.shouldBe` 15
