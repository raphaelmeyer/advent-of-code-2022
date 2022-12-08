{-# LANGUAGE OverloadedStrings #-}

module Day08.TreeHouseSpec where

import qualified Data.Text as Text
import qualified Day08.TreeHouse as TreeHouse
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "30373",
    "25512",
    "65332",
    "33549",
    "35390"
  ]

spec :: Spec
spec = do
  describe "Treetop Tree House" $ do
    let grid = TreeHouse.parseInput exampleInput

    it "should count visible trees" $ do
      TreeHouse.countVisible grid `shouldBe` 21

    it "should look from left" $ do
      TreeHouse.lookFromLeft grid !! 1 `shouldBe` [True, True, False, False, False]
      TreeHouse.lookFromLeft grid !! 3 `shouldBe` [True, False, True, False, True]

    it "should look from right" $ do
      TreeHouse.lookFromRight grid !! 1 `shouldBe` [False, False, True, False, True]
      TreeHouse.lookFromRight grid !! 2 `shouldBe` [True, True, False, True, True]

    it "should look from top" $ do
      TreeHouse.lookFromTop grid !! 0 `shouldBe` [True, True, True, True, True]
      TreeHouse.lookFromTop grid !! 1 `shouldBe` [False, True, True, False, False]
      TreeHouse.lookFromTop grid !! 2 `shouldBe` [True, False, False, False, False]
      TreeHouse.lookFromTop grid !! 3 `shouldBe` [False, False, False, False, True]
      TreeHouse.lookFromTop grid !! 4 `shouldBe` [False, False, False, True, False]

    it "should look from bottom" $ do
      TreeHouse.lookFromBottom grid !! 4 `shouldBe` [True, True, True, True, True]
      TreeHouse.lookFromBottom grid !! 3 `shouldBe` [False, False, True, False, True]
      TreeHouse.lookFromBottom grid !! 2 `shouldBe` [True, False, False, False, False]
      TreeHouse.lookFromBottom grid !! 1 `shouldBe` [False, False, False, False, False]
      TreeHouse.lookFromBottom grid !! 0 `shouldBe` [False, False, False, False, False]

    describe "Scenic Score" $ do
      let forest = TreeHouse.makeForest grid

      it "should find a tree's scenic score" $ do
        TreeHouse.count TreeHouse.up 5 (2, 1) forest `shouldBe` 1
        TreeHouse.count TreeHouse.left 5 (2, 1) forest `shouldBe` 1
        TreeHouse.count TreeHouse.right 5 (2, 1) forest `shouldBe` 2
        TreeHouse.count TreeHouse.down 5 (2, 1) forest `shouldBe` 2

        TreeHouse.scenicScore forest (2, 1) `shouldBe` 4
        TreeHouse.scenicScore forest (2, 3) `shouldBe` 8

      it "should find the highest scenic score" $ do
        TreeHouse.highestScenicScore forest `shouldBe` 8
