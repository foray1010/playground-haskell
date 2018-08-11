module Homework8Spec where

import qualified Test.Hspec as Hspec

import qualified Homework8.Employee as E
import qualified Homework8.Homework8 as HW

fixture1 = E.Emp {
  E.empName = "a",
  E.empFun = 1
}
fixture2 = E.GL [E.Emp {
  E.empName = "b",
  E.empFun = 2
}] 2
fixture3 = E.GL [E.Emp {
  E.empName = "c",
  E.empFun = 3
}] 3

spec :: Hspec.Spec
spec = Hspec.describe "Homework8" $ do
  Hspec.it "ex1.1: glCons" $
    HW.glCons fixture1 fixture2
      `Hspec.shouldBe`
      E.GL [E.Emp {
        E.empName = "b",
        E.empFun = 2
      }, E.Emp {
        E.empName = "a",
        E.empFun = 1
      }] 3

  Hspec.it "ex1.2: Monoid GuestList" $
    fixture2 <> fixture2
      `Hspec.shouldBe`
      E.GL [E.Emp {
        E.empName = "b",
        E.empFun = 2
      }, E.Emp {
        E.empName = "b",
        E.empFun = 2
      }] 4

  Hspec.it "ex1.3: moreFun" $ do
    HW.moreFun fixture2 fixture3 `Hspec.shouldBe` fixture3
    HW.moreFun fixture3 fixture2 `Hspec.shouldBe` fixture3
