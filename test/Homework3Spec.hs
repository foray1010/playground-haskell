module Homework3Spec (homework3Spec) where

import Test.Hspec

import Homework3.Homework3

homework3Spec :: Spec
homework3Spec = describe "Homework3" $
  it "ex1: skips" $ do
    skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]
    skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
    skips [1] `shouldBe` [[1]]
    skips [True,False] `shouldBe` [[True,False], [False]]
    skips [] `shouldBe` []
