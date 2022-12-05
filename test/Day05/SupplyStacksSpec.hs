{-# LANGUAGE OverloadedStrings #-}

module Day05.SupplyStacksSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Day05.SupplyStacks as Day05
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "    [D]    ",
    "[N] [C]    ",
    "[Z] [M] [P]",
    " 1   2   3 ",
    "",
    "move 1 from 2 to 1",
    "move 3 from 1 to 3",
    "move 2 from 2 to 1",
    "move 1 from 1 to 2"
  ]

spec :: Spec
spec = do
  describe "Supply Stacks" $ do
    let (stacks, procedure) = Day05.parseInput exampleInput

    describe "Parse input" $ do
      it "should parse crate stacks" $ do
        Map.size stacks `shouldBe` 3
        stacks Map.!? 1 `shouldBe` Just "NZ"
        stacks Map.!? 2 `shouldBe` Just "DCM"
        stacks Map.!? 3 `shouldBe` Just "P"

      it "should parse move procedures" $ do
        length procedure `shouldBe` 4
        procedure !! 1 `shouldBe` Day05.Move {Day05.getCrates = 3, Day05.getFrom = 1, Day05.getTo = 3}
        procedure !! 2 `shouldBe` Day05.Move {Day05.getCrates = 2, Day05.getFrom = 2, Day05.getTo = 1}

    it "should rearrange crates" $ do
      let first = Day05.rearrange9000' stacks (procedure !! 0)
      Map.lookup 1 first `shouldBe` (Just "DNZ")
      Map.lookup 2 first `shouldBe` (Just "CM")
      Map.lookup 3 first `shouldBe` (Just "P")

      let next = Day05.rearrange9000' first (procedure !! 1)
      Map.lookup 1 next `shouldBe` (Just [])
      Map.lookup 2 next `shouldBe` (Just "CM")
      Map.lookup 3 next `shouldBe` (Just "ZNDP")

    it "should rearrange crate stacks" $ do
      let result = Day05.rearrange9000 stacks procedure
      let message = Day05.topCrates result

      message `shouldBe` "CMZ"

    it "should rearrange crates with crane 9001" $ do
      let first = Day05.rearrange9001' stacks (procedure !! 0)
      let next = Day05.rearrange9001' first (procedure !! 1)
      Map.lookup 1 next `shouldBe` (Just [])
      Map.lookup 2 next `shouldBe` (Just "CM")
      Map.lookup 3 next `shouldBe` (Just "DNZP")

    it "should rearrange crate stacks with crane 9001" $ do
      let result = Day05.rearrange9001 stacks procedure
      let message = Day05.topCrates result

      message `shouldBe` "MCD"