module Homework3Spec (homework3Spec) where

import Test.Hspec

import Homework3.Homework3

homework3Spec :: Spec
homework3Spec = describe "Homework3" $ do
  it "ex1: skips" $ do
    skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]
    skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
    skips [1] `shouldBe` [[1]]
    skips [True, False] `shouldBe` [[True, False], [False]]
    length (skips []) `shouldBe` 0

  it "ex2: localMaxima" $ do
    localMaxima [2, 9, 5, 6, 1] `shouldBe` [9, 6]
    localMaxima [2, 3, 4, 1, 5] `shouldBe` [4]
    localMaxima [1, 2, 3, 4, 5] `shouldBe` []
    localMaxima [1, 2] `shouldBe` []
    localMaxima [1] `shouldBe` []
    localMaxima [] `shouldBe` []
