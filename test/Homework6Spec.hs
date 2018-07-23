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

  Hspec.it "ex6: x" $
    show HW.x
      `Hspec.shouldBe`
      "[0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]"

  Hspec.it "ex6: Stream + Stream" $
    show (HW.nats + HW.nats)
      `Hspec.shouldBe`
      "[0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38]"

  Hspec.it "ex6: Stream negate" $
    show (negate HW.nats)
      `Hspec.shouldBe`
      "[0,-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-17,-18,-19]"

  Hspec.it "ex6: Stream * Stream" $
    show (HW.nats * HW.nats)
      `Hspec.shouldBe`
      "[0,0,1,4,10,20,35,56,84,120,165,220,286,364,455,560,680,816,969,1140]"

  Hspec.it "ex6: fibs3" $
    show HW.fibs3
      `Hspec.shouldBe`
      "[0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181]"

  Hspec.it "ex7: Matrix + Matrix" $
    HW.Matrix (1, 2, 3, 4) + HW.Matrix (2, 0, 1, 2)
      `Hspec.shouldBe`
      HW.Matrix (3, 2, 4, 6)

  Hspec.it "ex7: Matrix - Matrix" $
    HW.Matrix (1, 2, 3, 4) - HW.Matrix (2, 0, 1, 2)
      `Hspec.shouldBe`
      HW.Matrix (-1, 2, 2, 2)

  Hspec.it "ex7: Matrix * Matrix" $
    HW.Matrix (1, 2, 3, 4) * HW.Matrix (2, 0, 1, 2)
      `Hspec.shouldBe`
      HW.Matrix (4, 4, 10, 8)

  Hspec.it "ex7: Matrix fromInteger" $
    (1 :: HW.Matrix)
      `Hspec.shouldBe`
      HW.Matrix (1, 1, 1, 1)

  Hspec.it "ex7: fib4" $ do
    HW.fib4 0 `Hspec.shouldBe` 0
    HW.fib4 1 `Hspec.shouldBe` 1
    HW.fib4 14 `Hspec.shouldBe` 377
