module Homework4Spec (homework4Spec) where

import Test.Hspec

import Homework4.Homework4

homework4Spec :: Spec
homework4Spec = describe "Homework4" $ do
  it "ex1: fun1" $ do
    fun1 [] `shouldBe` 1
    fun1 [0] `shouldBe` -2
    fun1 [7, 8, 9, 10, 12] `shouldBe` 480

  it "ex1: fun2" $ do
    fun2 0 `shouldBe` 0
    fun2 1 `shouldBe` 0
    fun2 2 `shouldBe` 2
    fun2 3 `shouldBe` 40
    fun2 4 `shouldBe` 6
    fun2 5 `shouldBe` 30
