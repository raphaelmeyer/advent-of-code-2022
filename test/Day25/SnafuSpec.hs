{-# LANGUAGE OverloadedStrings #-}

module Day25.SnafuSpec where

import qualified Data.Text as Text
import qualified Day25.Snafu as Snafu
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "1=-0-2",
    "12111",
    "2=0=",
    "21",
    "2=01",
    "111",
    "20012",
    "112",
    "1=-1=",
    "1-12",
    "12",
    "1=",
    "122"
  ]

spec :: Spec
spec = do
  describe "Full of Hot Air" $ do
    let input = Snafu.parseInput exampleInput

    it "should parse the input" $ do
      length input `shouldBe` 13

      (input !! 0) `shouldBe` [Snafu.Two, Snafu.Minus, Snafu.Zero, Snafu.Minus, Snafu.DoubleMinus, Snafu.One]
      (input !! 2) `shouldBe` [Snafu.DoubleMinus, Snafu.Zero, Snafu.DoubleMinus, Snafu.Two]

      Snafu.snprint (input !! 0) `shouldBe` "1=-0-2"
      Snafu.snprint (input !! 6) `shouldBe` "20012"

    it "should add snafu numbers" $ do
      Snafu.snprint (Snafu.snadd (input !! 0) (input !! 8)) `shouldBe` "1=2-00"
      Snafu.snprint (Snafu.snadd (input !! 8) (input !! 0)) `shouldBe` "1=2-00"

    it "should sum up snafu numbers" $ do
      (Snafu.snprint . Snafu.snum $ input) `shouldBe` "2=-1=0"
