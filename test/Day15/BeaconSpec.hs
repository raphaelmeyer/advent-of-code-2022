{-# LANGUAGE OverloadedStrings #-}

module Day15.BeaconSpec where

import qualified Data.Text as Text
import qualified Day15.Beacon as Beacon
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "Sensor at x=2, y=18: closest beacon is at x=-2, y=15",
    "Sensor at x=9, y=16: closest beacon is at x=10, y=16",
    "Sensor at x=13, y=2: closest beacon is at x=15, y=3",
    "Sensor at x=12, y=14: closest beacon is at x=10, y=16",
    "Sensor at x=10, y=20: closest beacon is at x=10, y=16",
    "Sensor at x=14, y=17: closest beacon is at x=10, y=16",
    "Sensor at x=8, y=7: closest beacon is at x=2, y=10",
    "Sensor at x=2, y=0: closest beacon is at x=2, y=10",
    "Sensor at x=0, y=11: closest beacon is at x=2, y=10",
    "Sensor at x=20, y=14: closest beacon is at x=25, y=17",
    "Sensor at x=17, y=20: closest beacon is at x=21, y=22",
    "Sensor at x=16, y=7: closest beacon is at x=15, y=3",
    "Sensor at x=14, y=3: closest beacon is at x=15, y=3",
    "Sensor at x=20, y=1: closest beacon is at x=15, y=3"
  ]

spec :: Spec
spec = do
  describe "Beacon Exclusion Zone" $ do
    let sensors = Beacon.parseInput exampleInput

    it "should parse the input" $ do
      length sensors `shouldBe` 14
      (sensors !! 11) `shouldBe` Beacon.Sensor (16, 7) (15, 3)

    it "should find area in a certain row covered by sensors" $ do
      Beacon.coveredInRow 10 sensors `shouldBe` 26

    it "should find the tuning frequency" $ do
      Beacon.tuningFrequency (0, 20) sensors `shouldBe` 56000011
