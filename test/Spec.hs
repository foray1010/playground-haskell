import Test.Hspec

import Homework1Spec
import Homework2Spec
import Homework3Spec
import Homework4Spec

main :: IO ()
main = hspec $ parallel $ describe "test suite" $ do
  homework1Spec
  homework2Spec
  homework3Spec
  homework4Spec
