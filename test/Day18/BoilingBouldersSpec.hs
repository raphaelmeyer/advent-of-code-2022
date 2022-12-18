{-# LANGUAGE OverloadedStrings #-}

module Day18.BoilingBouldersSpec where

import qualified Data.Text as Text
import qualified Day18.BoilingBoulders as Boulder
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "2,2,2",
    "1,2,2",
    "3,2,2",
    "2,1,2",
    "2,3,2",
    "2,2,1",
    "2,2,3",
    "2,2,4",
    "2,2,6",
    "1,2,5",
    "3,2,5",
    "2,1,5",
    "2,3,5"
  ]

spec :: Spec
spec = do
  describe "Boiling Boulders" $ do
    let boulder = Boulder.parseInput exampleInput

    it "should measure the surface" $ do
      Boulder.surface boulder `shouldBe` 64

    it "should measure the outer surface" $ do
      Boulder.outerSurface boulder `shouldBe` 58
