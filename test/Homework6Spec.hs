module Homework6Spec where

import qualified Test.Hspec as Hspec

import qualified Homework6.Homework6 as HW

genStream :: a -> HW.Stream a
genStream n = HW.Cons n (genStream n)

spec :: Hspec.Spec
spec = Hspec.describe "Homework6" $ do
  Hspec.it "ex1: fib" $ do
    HW.fib 0 `Hspec.shouldBe` 0
    HW.fib 1 `Hspec.shouldBe` 1
    HW.fib 14 `Hspec.shouldBe` 377

  Hspec.it "ex1: fibs1" $
    take 15 HW.fibs1
      `Hspec.shouldBe`
      [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]

  Hspec.it "ex1: fibs2" $
    take 15 HW.fibs2
      `Hspec.shouldBe`
      [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]

  Hspec.it "ex2: Stream" $
    show (genStream 0)
      `Hspec.shouldBe`
      "[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]"
