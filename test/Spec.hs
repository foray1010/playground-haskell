import Test.Hspec

import Homework1Spec
import Homework2Spec

main :: IO ()
main = hspec $ parallel $ describe "test suite" $ do
  homework1Spec
  homework2Spec
