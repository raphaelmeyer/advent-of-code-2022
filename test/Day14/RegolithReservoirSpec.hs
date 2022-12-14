{-# LANGUAGE OverloadedStrings #-}

module Day14.RegolithReservoirSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Day14.RegolithReservoir as Regolith
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "498,4 -> 498,6 -> 496,6",
    "503,4 -> 502,4 -> 502,9 -> 494,9"
  ]

spec :: Spec
spec = do
  describe "Regolith Reservoir" $ do
    let cave = Regolith.parseInput exampleInput

    it "should parseInput" $ do
      let rocks = Regolith.rocks cave
      Map.lookup (496, 6) rocks `shouldBe` Just Regolith.Rock
      Map.lookup (498, 5) rocks `shouldBe` Just Regolith.Rock
      Map.lookup (495, 9) rocks `shouldBe` Just Regolith.Rock
      Map.lookup (502, 7) rocks `shouldBe` Just Regolith.Rock
      Map.lookup (503, 4) rocks `shouldBe` Just Regolith.Rock

      Map.lookup (497, 5) rocks `shouldBe` Nothing
      Map.lookup (498, 3) rocks `shouldBe` Nothing
      Map.lookup (504, 4) rocks `shouldBe` Nothing

    it "should find the abyss" $ do
      Regolith.abyss cave `shouldBe` 9

    it "should drop sand" $ do
      let cave' = Regolith.dropSand cave
      cave' `shouldNotBe` Nothing

      let rocks = maybe Map.empty Regolith.rocks cave'
      Map.lookup (500, 7) rocks `shouldBe` Nothing
      Map.lookup (500, 8) rocks `shouldBe` Just Regolith.Sand

    it "should drop some more sand" $ do
      let cave' = iterate' 5 Regolith.dropSand cave
      cave' `shouldNotBe` Nothing

      let rocks = maybe Map.empty Regolith.rocks cave'
      Map.lookup (499, 7) rocks `shouldBe` Nothing
      Map.lookup (500, 7) rocks `shouldBe` Just Regolith.Sand
      Map.lookup (501, 7) rocks `shouldBe` Nothing
      Map.lookup (497, 8) rocks `shouldBe` Nothing
      Map.lookup (498, 8) rocks `shouldBe` Just Regolith.Sand
      Map.lookup (501, 8) rocks `shouldBe` Just Regolith.Sand

    it "should flood the cave with sand" $ do
      let flooded = Regolith.floodCave cave
      Regolith.sand flooded `shouldBe` 24

-- helper

iterate' :: Int -> (a -> Maybe a) -> a -> Maybe a
iterate' 0 _ a = Just a
iterate' n f a = case f a of
  Just b -> iterate' (n - 1) f b
  Nothing -> Nothing
