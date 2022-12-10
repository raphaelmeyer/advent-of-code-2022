{-# LANGUAGE OverloadedStrings #-}

module Day10.CathodeRayTubeSpec where

import qualified Data.Text as Text
import qualified Day10.CathodeRayTube as Tube
import Test.Hspec

spec :: Spec
spec = do
  describe "Cathode-Ray Tube" $ do
    let code = Tube.parseInput exampleInput
    let x = Tube.evaluateX code

    it "should calculate the signal strength" $ do
      Tube.signalValue 20 x `shouldBe` 420
      Tube.signalValue 60 x `shouldBe` 1140
      Tube.signalValue 100 x `shouldBe` 1800
      Tube.signalValue 140 x `shouldBe` 2940
      Tube.signalValue 180 x `shouldBe` 2880
      Tube.signalValue 220 x `shouldBe` 3960

    it "should return the sum of signal strength up to the 220th cycle" $ do
      Tube.sumSignal x `shouldBe` 13140

    it "should draw pixels" $ do
      -- ##..##..##..##..##..##..##..##..##..##..
      Tube.drawPixel x 0 `shouldBe` True
      Tube.drawPixel x 1 `shouldBe` True
      Tube.drawPixel x 2 `shouldBe` False
      Tube.drawPixel x 39 `shouldBe` False

      -- ###...###...###...###...###...###...###.
      Tube.drawPixel x 40 `shouldBe` True
      Tube.drawPixel x 78 `shouldBe` True
      Tube.drawPixel x 79 `shouldBe` False

      -- ####....####....####....####....####....
      -- #####.....#####.....#####.....#####.....
      -- ######......######......######......####
      -- #######.......#######.......#######.....
      Tube.drawPixel x 234 `shouldBe` True
      Tube.drawPixel x 235 `shouldBe` False

exampleInput :: [Text.Text]
exampleInput =
  [ "addx 15",
    "addx -11",
    "addx 6",
    "addx -3",
    "addx 5",
    "addx -1",
    "addx -8",
    "addx 13",
    "addx 4",
    "noop",
    "addx -1",
    "addx 5",
    "addx -1",
    "addx 5",
    "addx -1",
    "addx 5",
    "addx -1",
    "addx 5",
    "addx -1",
    "addx -35",
    "addx 1",
    "addx 24",
    "addx -19",
    "addx 1",
    "addx 16",
    "addx -11",
    "noop",
    "noop",
    "addx 21",
    "addx -15",
    "noop",
    "noop",
    "addx -3",
    "addx 9",
    "addx 1",
    "addx -3",
    "addx 8",
    "addx 1",
    "addx 5",
    "noop",
    "noop",
    "noop",
    "noop",
    "noop",
    "addx -36",
    "noop",
    "addx 1",
    "addx 7",
    "noop",
    "noop",
    "noop",
    "addx 2",
    "addx 6",
    "noop",
    "noop",
    "noop",
    "noop",
    "noop",
    "addx 1",
    "noop",
    "noop",
    "addx 7",
    "addx 1",
    "noop",
    "addx -13",
    "addx 13",
    "addx 7",
    "noop",
    "addx 1",
    "addx -33",
    "noop",
    "noop",
    "noop",
    "addx 2",
    "noop",
    "noop",
    "noop",
    "addx 8",
    "noop",
    "addx -1",
    "addx 2",
    "addx 1",
    "noop",
    "addx 17",
    "addx -9",
    "addx 1",
    "addx 1",
    "addx -3",
    "addx 11",
    "noop",
    "noop",
    "addx 1",
    "noop",
    "addx 1",
    "noop",
    "noop",
    "addx -13",
    "addx -19",
    "addx 1",
    "addx 3",
    "addx 26",
    "addx -30",
    "addx 12",
    "addx -1",
    "addx 3",
    "addx 1",
    "noop",
    "noop",
    "noop",
    "addx -9",
    "addx 18",
    "addx 1",
    "addx 2",
    "noop",
    "noop",
    "addx 9",
    "noop",
    "noop",
    "noop",
    "addx -1",
    "addx 2",
    "addx -37",
    "addx 1",
    "addx 3",
    "noop",
    "addx 15",
    "addx -21",
    "addx 22",
    "addx -6",
    "addx 1",
    "noop",
    "addx 2",
    "addx 1",
    "noop",
    "addx -10",
    "noop",
    "noop",
    "addx 20",
    "addx 1",
    "addx 2",
    "addx 2",
    "addx -6",
    "addx -11",
    "noop",
    "noop",
    "noop"
  ]
