module Homework5Spec (homework5Spec) where

import Test.Hspec

import Homework5.ExprT
import Homework5.Homework5
import Homework5.Parser

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

homework5Spec :: Spec
homework5Spec = describe "Homework5" $ do
  it "ex1: eval" $ do
    eval (Lit 1) `shouldBe` 1
    eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20

  it "ex2: evalStr" $ do
    evalStr "(2+3)*4" `shouldBe` Just 20
    evalStr "2+3*4" `shouldBe` Just 14
    evalStr "2+3*" `shouldBe` Nothing

  it "ex3: reify" $
    reify (mul (add (lit 2) (lit 3)) (lit 4))
      `shouldBe` Mul (Add (Lit 2) (Lit 3)) (Lit 4)

  it "ex4: testInteger" $
    testInteger `shouldBe` Just (-7)

  it "ex4: testBool" $
    testBool `shouldBe` Just True

  it "ex4: testMM" $
    testMM `shouldBe` Just (MinMax 5)

  it "ex4: testSat" $
    testSat `shouldBe` Just (Mod7 5)
