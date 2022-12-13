{-# LANGUAGE OverloadedStrings #-}

module Day12.HillClimbingSpec where

import qualified Data.Text as Text
import qualified Day12.HillClimbing as Hill
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "Sabqponm",
    "abcryxxl",
    "accszExk",
    "acctuvwj",
    "abdefghi"
  ]

spec :: Spec
spec = do
  describe "Hill Climbing Algorithm" $ do
    let area = Hill.parseInput exampleInput

    it "should parse input" $ do
      Hill.atArea area (0, 0) `shouldBe` (Just 0)
      Hill.atArea area (5, 2) `shouldBe` (Just 25)
      Hill.atArea area (3, 2) `shouldBe` (Just 18)
      Hill.atArea area (7, 4) `shouldBe` (Just 8)

      Hill.start area `shouldBe` (0, 0)
      Hill.goal area `shouldBe` (5, 2)

    it "should find the shortest climb" $ do
      Hill.shortestPath Hill.climb area `shouldBe` 31

    it "should find the shortest hike" $ do
      Hill.shortestPath Hill.hike area `shouldBe` 29