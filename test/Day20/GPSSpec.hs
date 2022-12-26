{-# LANGUAGE OverloadedStrings #-}

module Day20.GPSSpec where

import qualified Data.List as List
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
      Gps.mixSum 1 1 input `shouldBe` 3

    it "should mix numbers" $ do
      let mixed = Gps.mixNTimes 1 1 input

      rotateZero mixed `shouldBe` [0, 3, -2, 1, 2, -3, 4]

      Gps.nthElement mixed 1000 `shouldBe` 4
      Gps.nthElement mixed 2000 `shouldBe` -3
      Gps.nthElement mixed 3000 `shouldBe` 2

    it "should decrypt grove coordinates" $ do
      (rotateZero . Gps.mixNTimes 811589153 1 $ input) `shouldBe` [0, -2434767459, 3246356612, -1623178306, 2434767459, 1623178306, 811589153]
      (rotateZero . Gps.mixNTimes 811589153 2 $ input) `shouldBe` [0, 2434767459, 1623178306, 3246356612, -2434767459, -1623178306, 811589153]

      (rotateZero . Gps.mixNTimes 811589153 10 $ input) `shouldBe` [0, -2434767459, 1623178306, 3246356612, -1623178306, 2434767459, 811589153]

rotateZero :: [Int] -> [Int]
rotateZero file = case List.elemIndex 0 file of
  Nothing -> []
  Just i -> (\(a, b) -> b ++ a) . List.splitAt i $ file
