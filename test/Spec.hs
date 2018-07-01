import Test.Hspec

import Homework1

main :: IO ()
main = hspec $ describe "Homework1" $ do
  it "toDigits" $ do
    toDigits 1234 `shouldBe` [1, 2, 3, 4]
    toDigits 0 `shouldBe` []
    toDigits (-17) `shouldBe` []

  it "toDigitsRev" $ do
    toDigitsRev 1234 `shouldBe` [4, 3, 2, 1]
    toDigitsRev 0 `shouldBe` []
    toDigitsRev (-17) `shouldBe` []

  it "doubleEveryOther" $ do
    doubleEveryOther [8, 7, 6, 5] `shouldBe` [16, 7, 12, 5]
    doubleEveryOther [1, 2, 3] `shouldBe` [1, 4, 3]
    doubleEveryOther [] `shouldBe` []

  it "sumDigits" $ do
    sumDigits [16, 7, 12, 5] `shouldBe` 22
    sumDigits [] `shouldBe` 0

  it "validate" $ do
    validate 4012888888881881 `shouldBe` True
    validate 4012888888881882 `shouldBe` False
    validate 0 `shouldBe` True
