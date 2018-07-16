module Homework5Spec where

import qualified Test.Hspec as Hspec

import qualified Homework5.ExprT as ExprT
import qualified Homework5.Homework5 as HW
import qualified Homework5.Parser as Parser
import qualified Homework5.StackVM as StackVM

testExp :: HW.Expr a => Maybe a
testExp = Parser.parseExp HW.lit HW.add HW.mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe HW.MinMax
testSat = testExp :: Maybe HW.Mod7
testProgram = testExp :: Maybe StackVM.Program

spec :: Hspec.Spec
spec = Hspec.describe "Homework5" $ do
  Hspec.it "ex1: eval" $ do
    HW.eval (ExprT.Lit 1) `Hspec.shouldBe` 1
    HW.eval (ExprT.Mul (ExprT.Add (ExprT.Lit 2) (ExprT.Lit 3)) (ExprT.Lit 4))
      `Hspec.shouldBe` 20

  Hspec.it "ex2: evalStr" $ do
    HW.evalStr "(2+3)*4" `Hspec.shouldBe` Just 20
    HW.evalStr "2+3*4" `Hspec.shouldBe` Just 14
    HW.evalStr "2+3*" `Hspec.shouldBe` Nothing

  Hspec.it "ex3: reify" $
    HW.reify (HW.mul (HW.add (HW.lit 2) (HW.lit 3)) (HW.lit 4))
      `Hspec.shouldBe`
      ExprT.Mul (ExprT.Add (ExprT.Lit 2) (ExprT.Lit 3)) (ExprT.Lit 4)

  Hspec.it "ex4: testInteger" $
    testInteger `Hspec.shouldBe` Just (-7)

  Hspec.it "ex4: testBool" $
    testBool `Hspec.shouldBe` Just True

  Hspec.it "ex4: testMM" $
    testMM `Hspec.shouldBe` Just (HW.MinMax 5)

  Hspec.it "ex4: testSat" $
    testSat `Hspec.shouldBe` Just (HW.Mod7 5)

  Hspec.it "ex5: testProgram" $
    testProgram `Hspec.shouldBe` Just [
      StackVM.PushI 3,
      StackVM.PushI (-4),
      StackVM.Mul,
      StackVM.PushI 5,
      StackVM.Add]
