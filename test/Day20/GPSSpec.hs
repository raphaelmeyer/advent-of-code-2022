{-# LANGUAGE OverloadedStrings #-}

module Day20.GPSSpec where

import qualified Data.Text as Text
import qualified Day20.GPS as Gps
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "1",
    "2",
    "-3",
    "3",
    "-2",
    "0",
    "4"
  ]

spec :: Spec
spec = do
  describe "Grove Positioning System" $ do
    let input = Gps.parseInput exampleInput

    it "should summarize mixed coords" $ do
      Gps.sumMix input `shouldBe` 3

    it "should mix numbers" $ do
      let mixed = Gps.mix input
      -- mixed `shouldBe` [1, 2, -3, 4, 0, 3, -2] or any rotation

      Gps.nthElement mixed 1000 `shouldBe` 4
      Gps.nthElement mixed 2000 `shouldBe` -3
      Gps.nthElement mixed 3000 `shouldBe` 2
