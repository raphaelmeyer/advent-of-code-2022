{-# LANGUAGE OverloadedStrings #-}

module Day13.DistressSignalSpec where

import qualified Data.Text as Text
import qualified Day13.DistressSignal as Signal
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "[1,1,3,1,1]",
    "[1,1,5,1,1]",
    "",
    "[[1],[2,3,4]]",
    "[[1],4]",
    "",
    "[9]",
    "[[8,7,6]]",
    "",
    "[[4,4],4,4]",
    "[[4,4],4,4,4]",
    "",
    "[7,7,7,7]",
    "[7,7,7]",
    "",
    "[]",
    "[3]",
    "",
    "[[[]]]",
    "[[]]",
    "",
    "[1,[2,[3,[4,[5,6,7]]]],8,9]",
    "[1,[2,[3,[4,[5,6,0]]]],8,9]",
    ""
  ]

spec :: Spec
spec = do
  describe "Hill Climbing Algorithm" $ do
    let signal = Signal.parseInput exampleInput

    it "should parse input" $ do
      Signal.left (signal !! 0) `shouldBe` Signal.List [Signal.Value 1, Signal.Value 1, Signal.Value 3, Signal.Value 1, Signal.Value 1]
      Signal.right (signal !! 2) `shouldBe` Signal.List [Signal.List [Signal.Value 8, Signal.Value 7, Signal.Value 6]]

    it "should check if packtes are in right order" $ do
      (signal !! 0) `shouldSatisfy` Signal.inOrder
      (signal !! 1) `shouldSatisfy` Signal.inOrder
      (signal !! 3) `shouldSatisfy` Signal.inOrder
      (signal !! 5) `shouldSatisfy` Signal.inOrder

      (signal !! 2) `shouldNotSatisfy` Signal.inOrder
      (signal !! 4) `shouldNotSatisfy` Signal.inOrder
      (signal !! 6) `shouldNotSatisfy` Signal.inOrder
      (signal !! 7) `shouldNotSatisfy` Signal.inOrder

    it "should find indices of packets in order" $ do
      Signal.indices signal `shouldBe` [1, 2, 4, 6]
      Signal.orderSum signal `shouldBe` 13

    it "should find the decoder key" $ do
      Signal.decoderKey signal `shouldBe` 140
