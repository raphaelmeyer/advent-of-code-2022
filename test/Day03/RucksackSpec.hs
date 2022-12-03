{-# LANGUAGE OverloadedStrings #-}

module Day03.RucksackSpec where

import qualified Data.Text as Text
import Day03.Rucksack as Day03
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "vJrwpWtwJgWrhcsFMMfFFhFp",
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
    "PmmdzqPrVvPwwTWBwg",
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
    "ttgJtRGJQctTZtZT",
    "CrZsJsPPZsGzwwsLwLmpwMDw"
  ]

spec :: Spec
spec = do
  describe "Rucksack Reorganization " $ do
    let input = parseInput exampleInput

    it "should parse the input" $ do
      (input !! 0) `shouldBe` ("vJrwpWtwJgWr", "hcsFMMfFFhFp")
      (input !! 1) `shouldBe` ("jqHRNqRjqzjGDLGL", "rsFMfFZSrLrFZsSL")
      (input !! 2) `shouldBe` ("PmmdzqPrV", "vPwwTWBwg")

    it "should find duplicate item" $ do
      duplicateItems input `shouldBe` ['p', 'L', 'P', 'v', 't', 's']

    it "should return the priority value" $ do
      priority 'a' `shouldBe` 1
      priority 'p' `shouldBe` 16
      priority 'z' `shouldBe` 26
      priority 'A' `shouldBe` 27
      priority 'L' `shouldBe` 38
      priority 'Z' `shouldBe` 52

    it "should calcuate the priority sum" $ do
      prioritySum input `shouldBe` 157
