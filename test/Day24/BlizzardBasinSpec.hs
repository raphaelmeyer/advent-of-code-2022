{-# LANGUAGE OverloadedStrings #-}

module Day24.BlizzardBasinSpec where

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Day24.BlizzardBasin as B
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "#.######",
    "#>>.<^<#",
    "#.<..<<#",
    "#>v.><>#",
    "#<^v^^>#",
    "######.#"
  ]

spec :: Spec
spec = do
  describe "Blizzard Basin" $ do
    describe "Parse input" $ do
      let s = B.parseInput exampleInput

      it "should find entry and exit" $ do
        B.positions s `shouldBe` [(0, -1)]
        B.goal s `shouldBe` (5, 4)

      it "should register all blizzards" $ do
        B.blizzards s `shouldContain` [((0, 0), B.MoveRight)]
        B.blizzards s `shouldContain` [((5, 1), B.MoveLeft)]
        B.blizzards s `shouldContain` [((1, 2), B.MoveDown)]
        B.blizzards s `shouldContain` [((4, 3), B.MoveUp)]

        List.lookup (3, 1) (B.blizzards s) `shouldBe` Nothing

      it "should find the exit" $ do
        B.goForExit s `shouldBe` 18

      it "should go back for the snack" $ do
        B.goForSnack s `shouldBe` 54
