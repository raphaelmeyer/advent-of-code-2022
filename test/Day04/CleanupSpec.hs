{-# LANGUAGE OverloadedStrings #-}

module Day04.CleanupSpec where

import qualified Data.Text as Text
import qualified Day04.Cleanup as Day04
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "2-4,6-8",
    "2-3,4-5",
    "5-7,7-9",
    "2-8,3-7",
    "6-6,4-6",
    "2-6,4-8"
  ]

spec :: Spec
spec = do
  describe "Camp Cleanup" $ do
    let input = Day04.parseInput exampleInput

    it "should parse input" $ do
      length input `shouldBe` 6
      (input !! 2) `shouldBe` Day04.Pair (5, 7) (7, 9)

    it "should find fully containing pairs" $ do
      Day04.countFullyContaining input `shouldBe` 2

    it "should count overlapping pairs" $ do
      Day04.countOverlapping input `shouldBe` 4
