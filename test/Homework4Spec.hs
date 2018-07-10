module Homework4Spec (homework4Spec) where

import Test.Hspec

import Homework4.Homework4

homework4Spec :: Spec
homework4Spec = describe "Homework4" $ do
  it "ex1: fun1" $ do
    fun1 [] `shouldBe` 1
    fun1 [0] `shouldBe` -2
    fun1 [7, 8, 9, 10, 12] `shouldBe` 480

  it "ex1: fun2" $ do
    fun2 0 `shouldBe` 0
    fun2 1 `shouldBe` 0
    fun2 2 `shouldBe` 2
    fun2 3 `shouldBe` 40
    fun2 4 `shouldBe` 6
    fun2 5 `shouldBe` 30

  {-
  it "ex2: foldTree" $ do
    foldTree "" `shouldBe` Leaf

    foldTree "A" `shouldBe` Node 0 Leaf 'A' Leaf

    foldTree "ABCDEFGHIJ" `shouldBe` Node 3
      (Node 2
        (Node 0 Leaf 'F' Leaf)
        'I'
        (Node 1 (Node 0 Leaf 'B' Leaf) 'C' Leaf))
      'J'
      (Node 2
        (Node 1 (Node 0 Leaf 'A' Leaf) 'G' Leaf)
        'H'
        (Node 1 (Node 0 Leaf 'D' Leaf) 'E' Leaf))
  -}

  it "ex3: xor" $ do
    xor [] `shouldBe` False
    xor [True] `shouldBe` True
    xor [False, True] `shouldBe` True
    xor [False, True, False] `shouldBe` True
    xor [False, True, False, False, True] `shouldBe` False

  it "ex3: map'" $ do
    map' show [0] `shouldBe` ["0"]
    map' show [0, 1] `shouldBe` ["0", "1"]

  it "ex3: myFoldl" $ do
    myFoldl (-) 2 [] `shouldBe` 2
    myFoldl (-) 2 [1] `shouldBe` 1
    myFoldl (-) 2 [1, 3] `shouldBe` -2

  it "ex4: cartProd" $
    cartProd [1, 2] ['a', 'b'] `shouldBe` [(1, 'a'), (1, 'b'), (2, 'a'), (2, 'b')]

  it "ex4: sieveSundaram" $ do
    sieveSundaram 0 `shouldBe` []
    sieveSundaram 1 `shouldBe` [3]
    sieveSundaram 10 `shouldBe` [3, 5, 7, 11, 13, 17, 19]
    sieveSundaram 20 `shouldBe` [3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41]
