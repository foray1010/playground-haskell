import Test.Hspec

import Homework1Spec

main :: IO ()
main = hspec $ parallel $ describe "test suite" homework1Spec
