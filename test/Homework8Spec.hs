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

spec :: Hspec.Spec
spec = Hspec.describe "Homework8" $
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
