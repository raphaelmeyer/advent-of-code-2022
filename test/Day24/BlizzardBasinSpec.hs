{-# LANGUAGE OverloadedStrings #-}

module Day24.BlizzardBasinSpec where

import qualified Data.Set as Set
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
        Set.toList (B.positions s) `shouldBe` [(0, -1)]
        B.goal s `shouldBe` (5, 4)

      it "should register all blizzards" $ do
        B.at (B.blizzards s) (0, 0) `shouldBe` B.MoveRight
        B.at (B.blizzards s) (5, 1) `shouldBe` B.MoveLeft
        B.at (B.blizzards s) (1, 2) `shouldBe` B.MoveDown
        B.at (B.blizzards s) (4, 3) `shouldBe` B.MoveUp

        B.at (B.blizzards s) (3, 1) `shouldBe` B.None

      it "should find the exit" $ do
        B.goForExit s `shouldBe` 18

      it "should go back for the snack" $ do
        B.goForSnack s `shouldBe` 54

      it "should check if a position is free at a given time" $ do
        (0, 0) `shouldSatisfy` B.checkEmpty s {B.time = 13}
        (3, 0) `shouldSatisfy` B.checkEmpty s {B.time = 13}
        (5, 0) `shouldSatisfy` B.checkEmpty s {B.time = 13}
        (1, 1) `shouldSatisfy` B.checkEmpty s {B.time = 13}
        (2, 1) `shouldSatisfy` B.checkEmpty s {B.time = 13}
        (5, 1) `shouldSatisfy` B.checkEmpty s {B.time = 13}
        (2, 2) `shouldSatisfy` B.checkEmpty s {B.time = 13}
        (5, 2) `shouldSatisfy` B.checkEmpty s {B.time = 13}
        (2, 3) `shouldSatisfy` B.checkEmpty s {B.time = 13}
        (3, 3) `shouldSatisfy` B.checkEmpty s {B.time = 13}

        (3, 1) `shouldNotSatisfy` B.checkEmpty s {B.time = 13}
        (5, 3) `shouldNotSatisfy` B.checkEmpty s {B.time = 13}
