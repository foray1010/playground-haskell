module Homework5Spec (homework5Spec) where

import Test.Hspec

import Homework5.ExprT
import Homework5.Homework5

homework5Spec :: Spec
homework5Spec = describe "Homework5" $
  it "ex1: eval" $ do
    eval (Lit 1) `shouldBe` 1
    eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20
