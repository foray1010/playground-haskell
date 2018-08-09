module Homework7Spec where

import qualified Data.Monoid as Monoid
import qualified Test.Hspec as Hspec

import qualified Homework7.Homework7 as HW
import qualified Homework7.Sized as Sized

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: HW.JoinList m a -> [a]
jlToList HW.Empty = []
jlToList (HW.Single _ a) = [a]
jlToList (HW.Append _ l1 l2) = jlToList l1 ++ jlToList l2

idealIndexJ i jl = jlToList jl !!? i

idealDropJ n jl = drop n (jlToList jl)
idealTakeJ n jl = take n (jlToList jl)

fixture1 = HW.Single (Monoid.Product 3) HW.Empty
fixture2 = HW.Append (Monoid.Product 5) HW.Empty HW.Empty
fixture3 = HW.Single (Sized.Size 1) 'a'
fixture4 = HW.Append (Sized.Size 2)
  (HW.Single (Sized.Size 1) 'b')
  (HW.Single (Sized.Size 1) 'c')

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
    jlToList(HW.dropJ 0 fixture3)
      `Hspec.shouldBe`
      idealDropJ 0 fixture3

    jlToList(HW.dropJ 1 fixture3)
      `Hspec.shouldBe`
      idealDropJ 1 fixture3

    jlToList(HW.dropJ 2 fixture3)
      `Hspec.shouldBe`
      idealDropJ 2 fixture3

    jlToList(HW.dropJ 0 fixture4)
      `Hspec.shouldBe`
      idealDropJ 0 fixture4

    jlToList(HW.dropJ 1 fixture4)
      `Hspec.shouldBe`
      idealDropJ 1 fixture4

    jlToList(HW.dropJ 2 fixture4)
      `Hspec.shouldBe`
      idealDropJ 2 fixture4

    jlToList(HW.dropJ 3 fixture4)
      `Hspec.shouldBe`
      idealDropJ 3 fixture4

  Hspec.it "ex2.3: takeJ" $ do
    jlToList(HW.takeJ 0 fixture3)
      `Hspec.shouldBe`
      idealTakeJ 0 fixture3

    jlToList(HW.takeJ 1 fixture3)
      `Hspec.shouldBe`
      idealTakeJ 1 fixture3

    jlToList(HW.takeJ 2 fixture3)
      `Hspec.shouldBe`
      idealTakeJ 2 fixture3

    jlToList(HW.takeJ 0 fixture4)
      `Hspec.shouldBe`
      idealTakeJ 0 fixture4

    jlToList(HW.takeJ 1 fixture4)
      `Hspec.shouldBe`
      idealTakeJ 1 fixture4

    jlToList(HW.takeJ 2 fixture4)
      `Hspec.shouldBe`
      idealTakeJ 2 fixture4

    jlToList(HW.takeJ 3 fixture4)
      `Hspec.shouldBe`
      idealTakeJ 3 fixture4

  Hspec.it "ex3: scoreLine" $
    HW.scoreLine "yay " HW.+++ HW.scoreLine "haskell!"
      `Hspec.shouldBe`
      HW.Append (HW.Score 23)
        (HW.Single (HW.Score 9) "yay ")
        (HW.Single (HW.Score 14) "haskell!")
