module Homework2Spec (homework2Spec) where

import Test.Hspec

import Homework2.Homework2
import Homework2.Log

homework2Spec :: Spec
homework2Spec = describe "Homework2" $ do
  it "ex1: parseMessage" $ do
    parseMessage "E 2 562 help help" `shouldBe`
      LogMessage (Error 2) 562 "help help"
    parseMessage "I 29 la la la" `shouldBe`
      LogMessage Info 29 "la la la"
    parseMessage "W 0 la la la" `shouldBe`
      LogMessage Warning 0 "la la la"
    parseMessage "This is not in the right format" `shouldBe`
      Unknown "This is not in the right format"

  it "ex1: parse" $
    shouldBe
      (parse (unlines [
        "E 2 562 help help",
        "I 29 la la la",
        "W 0 la la la",
        "This is not in the right format"]))
      [LogMessage (Error 2) 562 "help help",
      LogMessage Info 29 "la la la",
      LogMessage Warning 0 "la la la",
      Unknown "This is not in the right format"]
