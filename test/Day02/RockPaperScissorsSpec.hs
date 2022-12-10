{-# LANGUAGE OverloadedStrings #-}

module Day02.RockPaperScissorsSpec where

import qualified Data.Text as Text
import Day02.RockPaperScissors as Day02
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "A Y",
    "B X",
    "C Z"
  ]

spec :: Spec
spec = do
  describe "Rock Paper Scissors" $ do
    let input = Day02.parseInput exampleInput

    it "should parse the input" $ do
      length input `shouldBe` 3
      input !! 1 `shouldBe` Strategy Paper Lose

    it "should calcuate the score for a round" $ do
      let rounds = guessShapes input
      roundScore (rounds !! 0) `shouldBe` 8
      roundScore (rounds !! 1) `shouldBe` 1
      roundScore (rounds !! 2) `shouldBe` 6

    it "should calculate the total score" $ do
      let guessed = guessShapes input
      totalScore guessed `shouldBe` 15

      let guide = chooseShapes input
      totalScore guide `shouldBe` 12

    it "should choose shape" $ do
      let shapes = chooseShapes input
      (shapes !! 0) `shouldBe` Round Rock Rock
      (shapes !! 1) `shouldBe` Round Paper Rock
      (shapes !! 2) `shouldBe` Round Scissors Rock
