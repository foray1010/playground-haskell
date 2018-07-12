module Homework5Spec (homework5Spec) where

import Test.Hspec

import Homework5.ExprT
import Homework5.Homework5

homework5Spec :: Spec
homework5Spec = describe "Homework5" $ do
  it "ex1: eval" $ do
    eval (Lit 1) `shouldBe` 1
    eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20

  it "ex2: evalStr" $ do
    evalStr "(2+3)*4" `shouldBe` Just 20
    evalStr "2+3*4" `shouldBe` Just 14
    evalStr "2+3*" `shouldBe` Nothing
