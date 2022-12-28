{-# LANGUAGE OverloadedStrings #-}

module Day23.UnstableDiffusionSpec where

import qualified Control.Monad.State as S
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Day23.UnstableDiffusion as E
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "....#..",
    "..###.#",
    "#...#.#",
    ".#...##",
    "#.###..",
    "##.#.##",
    ".#..#.."
  ]

spec :: Spec
spec = do
  describe "Unstable Diffusion" $ do
    let elves = E.parseInput exampleInput

    it "should parse the input" $ do
      elves `shouldSatisfy` Set.member (4, 0)
      elves `shouldNotSatisfy` Set.member (5, 1)
      elves `shouldSatisfy` Set.member (6, 1)
      elves `shouldSatisfy` Set.member (1, 6)
      elves `shouldNotSatisfy` Set.member (2, 6)

      Set.size elves `shouldBe` 22

    it "should count empty ground tiles" $ do
      E.countEmpty (E.State elves []) `shouldBe` 27

    it "should check the progress toward covering enough ground" $ do
      E.checkProgress elves `shouldBe` 110

  describe "Small example" $ do
    let elves = E.parseInput [".....", "..##.", "..#..", ".....", "..##.", "....."]

    it "should parse the small example input" $ do
      Set.toList elves `shouldMatchList` [(2, 1), (3, 1), (2, 2), (2, 4), (3, 4)]

    it "should execute the spreading process" $ do
      let rounds = iterate (S.execState E.elfRound) (E.State elves [E.North, E.South, E.West, E.East])

      let round1 = E.elfPositions (rounds !! 1)
      Set.toList round1 `shouldMatchList` [(2, 0), (3, 0), (2, 2), (3, 3), (2, 4)]

      let round2 = E.elfPositions (rounds !! 2)
      Set.toList round2 `shouldMatchList` [(2, 1), (3, 1), (1, 2), (4, 3), (2, 5)]

      let round3 = E.elfPositions (rounds !! 3)
      Set.toList round3 `shouldMatchList` [(2, 0), (4, 1), (0, 2), (4, 3), (2, 5)]

      let round4 = E.elfPositions (rounds !! 4)
      Set.toList round4 `shouldMatchList` [(2, 0), (4, 1), (0, 2), (4, 3), (2, 5)]
