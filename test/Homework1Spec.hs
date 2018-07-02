module Homework1Spec (homework1Spec) where

import Test.Hspec

import Homework1

homework1Spec :: Spec
homework1Spec = describe "Homework1" $ do
  it "ex1: toDigits" $ do
    toDigits 1234 `shouldBe` [1, 2, 3, 4]
    toDigits 0 `shouldBe` []
    toDigits (-17) `shouldBe` []

  it "ex1: toDigitsRev" $ do
    toDigitsRev 1234 `shouldBe` [4, 3, 2, 1]
    toDigitsRev 0 `shouldBe` []
    toDigitsRev (-17) `shouldBe` []

  it "ex2: doubleEveryOther" $ do
    doubleEveryOther [8, 7, 6, 5] `shouldBe` [16, 7, 12, 5]
    doubleEveryOther [1, 2, 3] `shouldBe` [1, 4, 3]
    doubleEveryOther [] `shouldBe` []

  it "ex3: sumDigits" $ do
    sumDigits [16, 7, 12, 5] `shouldBe` 22
    sumDigits [] `shouldBe` 0

  it "ex4: validate" $ do
    validate 4012888888881881 `shouldBe` True
    validate 4012888888881882 `shouldBe` False
    validate 0 `shouldBe` True

  it "ex5: hanoi" $ do
    hanoi 0 "a" "b" "a" `shouldBe` []
    hanoi 1 "a" "b" "c" `shouldBe` [("a", "b")]
    hanoi 2 "a" "b" "c" `shouldBe` [("a", "c"), ("a", "b"), ("c", "b")]
    hanoi 3 "a" "b" "c" `shouldBe` [
      ("a", "b"),
      ("a", "c"),
      ("b", "c"),
      ("a", "b"),
      ("c", "a"),
      ("c", "b"),
      ("a", "b")]
    hanoi 4 "a" "b" "c" `shouldBe` [
      ("a", "c"),
      ("a", "b"),
      ("c", "b"),
      ("a", "c"),
      ("b", "a"),
      ("b", "c"),
      ("a", "c"),
      ("a", "b"),
      ("c", "b"),
      ("c", "a"),
      ("b", "a"),
      ("c", "b"),
      ("a", "c"),
      ("a", "b"),
      ("c", "b")]
