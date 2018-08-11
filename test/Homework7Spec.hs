module Homework7Spec where

import qualified Data.Monoid as Monoid
import qualified Test.Hspec as Hspec

import qualified Homework7.Buffer as Buffer
import qualified Homework7.Homework7 as HW
import qualified Homework7.Sized as Sized

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

idealIndexJ i jl = HW.jlToList jl !!? i

idealDropJ n jl = drop n (HW.jlToList jl)
idealTakeJ n jl = take n (HW.jlToList jl)

fixture1 = HW.Single (Monoid.Product 3) HW.Empty
fixture2 = HW.Append (Monoid.Product 5) HW.Empty HW.Empty
fixture3 = HW.Single (Sized.Size 1) 'a'
fixture4 = HW.Append (Sized.Size 2)
  (HW.Single (Sized.Size 1) 'b')
  (HW.Single (Sized.Size 1) 'c')
fixture5 = HW.Append (HW.Score 5, Sized.Size 2)
  (HW.Single (HW.Score 1, Sized.Size 1) "a")
  (HW.Single (HW.Score 4, Sized.Size 1) "f")

spec :: Hspec.Spec
spec = Hspec.describe "Homework7" $ do
  Hspec.it "ex1: tag" $ do
    HW.tag fixture1
      `Hspec.shouldBe` 3

    HW.tag fixture2
      `Hspec.shouldBe` 5

  Hspec.it "ex1: (+++)" $
    HW.tag (fixture1 HW.+++ fixture2)
      `Hspec.shouldBe` 15

  Hspec.it "ex2.1: indexJ" $ do
    HW.indexJ 0 fixture3
      `Hspec.shouldBe`
      idealIndexJ 0 fixture3

    HW.indexJ 1 fixture3
      `Hspec.shouldBe`
      idealIndexJ 1 fixture3

    HW.indexJ 0 fixture4
      `Hspec.shouldBe`
      idealIndexJ 0 fixture4

    HW.indexJ 1 fixture4
      `Hspec.shouldBe`
      idealIndexJ 1 fixture4

    HW.indexJ 2 fixture4
      `Hspec.shouldBe`
      idealIndexJ 2 fixture4

  Hspec.it "ex2.2: dropJ" $ do
    HW.jlToList(HW.dropJ 0 fixture3)
      `Hspec.shouldBe`
      idealDropJ 0 fixture3

    HW.jlToList(HW.dropJ 1 fixture3)
      `Hspec.shouldBe`
      idealDropJ 1 fixture3

    HW.jlToList(HW.dropJ 2 fixture3)
      `Hspec.shouldBe`
      idealDropJ 2 fixture3

    HW.jlToList(HW.dropJ 0 fixture4)
      `Hspec.shouldBe`
      idealDropJ 0 fixture4

    HW.jlToList(HW.dropJ 1 fixture4)
      `Hspec.shouldBe`
      idealDropJ 1 fixture4

    HW.jlToList(HW.dropJ 2 fixture4)
      `Hspec.shouldBe`
      idealDropJ 2 fixture4

    HW.jlToList(HW.dropJ 3 fixture4)
      `Hspec.shouldBe`
      idealDropJ 3 fixture4

  Hspec.it "ex2.3: takeJ" $ do
    HW.jlToList(HW.takeJ 0 fixture3)
      `Hspec.shouldBe`
      idealTakeJ 0 fixture3

    HW.jlToList(HW.takeJ 1 fixture3)
      `Hspec.shouldBe`
      idealTakeJ 1 fixture3

    HW.jlToList(HW.takeJ 2 fixture3)
      `Hspec.shouldBe`
      idealTakeJ 2 fixture3

    HW.jlToList(HW.takeJ 0 fixture4)
      `Hspec.shouldBe`
      idealTakeJ 0 fixture4

    HW.jlToList(HW.takeJ 1 fixture4)
      `Hspec.shouldBe`
      idealTakeJ 1 fixture4

    HW.jlToList(HW.takeJ 2 fixture4)
      `Hspec.shouldBe`
      idealTakeJ 2 fixture4

    HW.jlToList(HW.takeJ 3 fixture4)
      `Hspec.shouldBe`
      idealTakeJ 3 fixture4

  Hspec.it "ex3: scoreLine" $
    HW.scoreLine "yay " HW.+++ HW.scoreLine "haskell!"
      `Hspec.shouldBe`
      HW.Append (HW.Score 23)
        (HW.Single (HW.Score 9) "yay ")
        (HW.Single (HW.Score 14) "haskell!")

  Hspec.it "ex4: Buffer.toString" $
    Buffer.toString fixture5
      `Hspec.shouldBe`
      "a\nf\n"

  Hspec.it "ex4: Buffer.fromString" $
    Buffer.fromString "a\nf\n"
      `Hspec.shouldBe`
      fixture5

  Hspec.it "ex4: Buffer.lines" $
    Buffer.line 1 fixture5
      `Hspec.shouldBe`
      Just "f"

  Hspec.it "ex4: Buffer.replaceLine" $
    Buffer.replaceLine 0 "b" fixture5
      `Hspec.shouldBe`
      HW.Append (HW.Score 7, Sized.Size 2)
        (HW.Single (HW.Score 3, Sized.Size 1) "b")
        (HW.Single (HW.Score 4, Sized.Size 1) "f")

  Hspec.it "ex4: Buffer.numLines" $
    Buffer.numLines fixture5 `Hspec.shouldBe` 2

  Hspec.it "ex4: Buffer.value" $
    Buffer.value fixture5 `Hspec.shouldBe` 5
