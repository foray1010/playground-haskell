module Homework5Spec (homework5Spec) where

import Test.Hspec

import Homework5.ExprT as ExprT
import Homework5.Homework5
import Homework5.Parser
import Homework5.StackVM as StackVM

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7
testProgram = testExp :: Maybe Program

homework5Spec :: Spec
homework5Spec = describe "Homework5" $ do
  it "ex1: eval" $ do
    eval (Lit 1) `shouldBe` 1
    eval (ExprT.Mul (ExprT.Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20

  it "ex2: evalStr" $ do
    evalStr "(2+3)*4" `shouldBe` Just 20
    evalStr "2+3*4" `shouldBe` Just 14
    evalStr "2+3*" `shouldBe` Nothing

  it "ex3: reify" $
    reify (mul (add (lit 2) (lit 3)) (lit 4))
      `shouldBe` ExprT.Mul (ExprT.Add (Lit 2) (Lit 3)) (Lit 4)

  it "ex4: testInteger" $
    testInteger `shouldBe` Just (-7)

  it "ex4: testBool" $
    testBool `shouldBe` Just True

  it "ex4: testMM" $
    testMM `shouldBe` Just (MinMax 5)

  it "ex4: testSat" $
    testSat `shouldBe` Just (Mod7 5)

  it "ex5: testProgram" $
    testProgram `shouldBe` Just [PushI 3, PushI (-4), StackVM.Mul, PushI 5, StackVM.Add]
