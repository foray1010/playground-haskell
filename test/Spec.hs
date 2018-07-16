import qualified Test.Hspec as Hspec

import qualified Homework1Spec as HW1Spec
import qualified Homework2Spec as HW2Spec
import qualified Homework3Spec as HW3Spec
import qualified Homework4Spec as HW4Spec
import qualified Homework5Spec as HW5Spec

main :: IO ()
main =
  Hspec.hspec $
  Hspec.parallel $
  Hspec.describe "test suite" $
  do
    HW1Spec.spec
    HW2Spec.spec
    HW3Spec.spec
    HW4Spec.spec
    HW5Spec.spec
