module Homework6Spec where

import qualified Test.Hspec as Hspec

import qualified Homework6.Homework6 as HW

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

  Hspec.it "ex2: fibs2" $
    take 15 HW.fibs2
      `Hspec.shouldBe`
      [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]

  Hspec.it "ex3: show Stream & ex4: streamRepeat" $
    show (HW.streamRepeat 0)
      `Hspec.shouldBe`
      "[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]"

  Hspec.it "ex4: streamMap" $
    show (HW.streamMap (+1) (HW.streamRepeat 0))
      `Hspec.shouldBe`
      "[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]"

  Hspec.it "ex4: streamFromSeed" $
    show (HW.streamFromSeed (+1) 0)
      `Hspec.shouldBe`
      "[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]"

  Hspec.it "ex5: nats" $
    show HW.nats
      `Hspec.shouldBe`
      "[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]"

  Hspec.it "ex5: interleaveStreams" $
    show (HW.interleaveStreams (HW.streamRepeat 0) (HW.streamRepeat 1))
      `Hspec.shouldBe`
      "[0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1]"

  Hspec.it "ex5: ruler" $
    show HW.ruler
      `Hspec.shouldBe`
      "[0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2]"
