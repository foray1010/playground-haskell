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

  it "ex2: insert" $ do
    insert (Unknown "1") Leaf `shouldBe` Leaf

    insert (LogMessage Info 1 "1") Leaf `shouldBe` Node Leaf (LogMessage Info 1 "1") Leaf

    insert
      (LogMessage Info 2 "2")
      (Node Leaf (LogMessage Info 1 "1") (Node Leaf (LogMessage Info 3 "3") Leaf))
      `shouldBe`
      Node Leaf (LogMessage Info 1 "1") (
        Node (Node Leaf (LogMessage Info 2 "2") Leaf) (LogMessage Info 3 "3") Leaf
      )

  it "ex3: build" $ do
    build [] `shouldBe` Leaf

    build [
      LogMessage Info 1 "1",
      LogMessage Info 2 "2",
      LogMessage Info 3 "3"]
      `shouldBe`
      Node (
        Node (Node Leaf (LogMessage Info 1 "1") Leaf) (LogMessage Info 2 "2") Leaf
      ) (LogMessage Info 3 "3") Leaf

  it "ex4: inOrder" $ do
    inOrder Leaf `shouldBe` []

    inOrder (
      Node (
        Node (Node Leaf (LogMessage Info 1 "1") Leaf) (LogMessage Info 2 "2") Leaf
      ) (LogMessage Info 3 "3") Leaf)
      `shouldBe`
      [
        LogMessage Info 1 "1",
        LogMessage Info 2 "2",
        LogMessage Info 3 "3"]

  it "ex5: whatWentWrong" $ do
    whatWentWrong [] `shouldBe` []

    whatWentWrong [
      LogMessage Info 6 "Completed armadillo processing",
      LogMessage Info 1 "Nothing to report",
      LogMessage (Error 99) 10 "Flange failed!",
      LogMessage Info 4 "Everything normal",
      LogMessage Info 11 "Initiating self-destruct sequence",
      LogMessage (Error 70) 3 "Way too many pickles",
      LogMessage (Error 65) 8 "Bad pickle-flange interaction detected",
      LogMessage Warning 5 "Flange is due for a check-up",
      LogMessage Info 7 "Out for lunch, back in two time steps",
      LogMessage (Error 20) 2 "Too many pickles",
      LogMessage Info 9 "Back from lunch"] `shouldBe` [
      "Way too many pickles",
      "Bad pickle-flange interaction detected",
      "Flange failed!"]
