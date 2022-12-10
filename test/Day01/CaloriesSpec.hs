{-# LANGUAGE OverloadedStrings #-}

module Day01.CaloriesSpec where

import qualified Data.Text as Text
import Day01.Calories as Calories
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "1000",
    "2000",
    "3000",
    "",
    "4000",
    "",
    "5000",
    "6000",
    "",
    "7000",
    "8000",
    "9000",
    "",
    "10000"
  ]

spec :: Spec
spec = do
  describe "Food Calories" $ do
    it "return maximum sum of calories carried by one elf" $ do
      let foods = Calories.parseInput exampleInput
      Calories.mostCalories foods `shouldBe` (24000 :: Int)

    it "should return calories sum of top three packages" $ do
      let foods = Calories.parseInput exampleInput
      Calories.topThreeSum foods `shouldBe` 45000
