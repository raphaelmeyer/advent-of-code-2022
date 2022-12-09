{-# LANGUAGE OverloadedStrings #-}

module Day09.RopeBridgeSpec where

import qualified Data.Text as Text
import qualified Day09.RopeBridge as RopeBridge
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "R 4",
    "U 4",
    "L 3",
    "D 1",
    "R 4",
    "D 1",
    "L 5",
    "R 2"
  ]

largerExample :: [Text.Text]
largerExample =
  [ "R 5",
    "U 8",
    "L 8",
    "D 3",
    "R 17",
    "D 10",
    "L 25",
    "U 20"
  ]

spec :: Spec
spec = do
  describe "Rope Bridge" $ do
    describe "Short Ropes" $ do
      let moves = RopeBridge.parseInput exampleInput

      it "should parse the input" $ do
        moves !! 2 `shouldBe` RopeBridge.Move RopeBridge.L 3

      it "should count all positions visited by tail" $ do
        RopeBridge.countVisitedByTail moves `shouldBe` 13

    describe "Long Ropes" $ do
      it "should count all positions visited by tail" $ do
        let moves = RopeBridge.parseInput largerExample
        RopeBridge.countVisitedByLongTail moves `shouldBe` 36
