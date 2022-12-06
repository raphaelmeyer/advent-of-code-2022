{-# LANGUAGE OverloadedStrings #-}

module Day06.TuningSpec where

import qualified Data.Text as Text
import qualified Day06.Tuning as Tuning
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "mjqjpqmgbljsphdztnvjfqwrcgsmlb",
    "bvwbjplbgvbhsrlpgdmjqwftvncz",
    "nppdvjthqldpwncqszvftbrmjlhg",
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
  ]

spec :: Spec
spec = do
  describe "Tuning Trouble" $ do
    it "should find the packet marker" $ do
      Tuning.packetMarker (exampleInput !! 0) `shouldBe` 7
      Tuning.packetMarker (exampleInput !! 1) `shouldBe` 5
      Tuning.packetMarker (exampleInput !! 2) `shouldBe` 6
      Tuning.packetMarker (exampleInput !! 3) `shouldBe` 10
      Tuning.packetMarker (exampleInput !! 4) `shouldBe` 11

    it "should find the message marker" $ do
      Tuning.messageMarker (exampleInput !! 0) `shouldBe` 19
      Tuning.messageMarker (exampleInput !! 1) `shouldBe` 23
      Tuning.messageMarker (exampleInput !! 2) `shouldBe` 23
      Tuning.messageMarker (exampleInput !! 3) `shouldBe` 29
      Tuning.messageMarker (exampleInput !! 4) `shouldBe` 26
