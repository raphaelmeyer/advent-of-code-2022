{-# LANGUAGE OverloadedStrings #-}

module Day11.KeepAwaySpec where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Day11.KeepAway as KeepAway
import qualified Day11.Monkey as M
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "Monkey 0:",
    "  Starting items: 79, 98",
    "  Operation: new = old * 19",
    "  Test: divisible by 23",
    "    If true: throw to monkey 2",
    "    If false: throw to monkey 3",
    "",
    "Monkey 1:",
    "  Starting items: 54, 65, 75, 74",
    "  Operation: new = old + 6",
    "  Test: divisible by 19",
    "    If true: throw to monkey 2",
    "    If false: throw to monkey 0",
    "",
    "Monkey 2:",
    "  Starting items: 79, 60, 97",
    "  Operation: new = old * old",
    "  Test: divisible by 13",
    "    If true: throw to monkey 1",
    "    If false: throw to monkey 3",
    "",
    "Monkey 3:",
    "  Starting items: 74",
    "  Operation: new = old + 3",
    "  Test: divisible by 17",
    "    If true: throw to monkey 0",
    "    If false: throw to monkey 1"
  ]

spec :: Spec
spec = do
  describe "Monkey in the Middle" $ do
    let monkeys = KeepAway.parseInput exampleInput

    it "should parse monkeys" $ do
      Map.size monkeys `shouldBe` 4
      Map.lookup 2 monkeys `shouldBe` Just (M.Monkey [79, 60, 97] (M.Mul M.Old M.Old) (M.Test 13 1 3) 0)

    describe "Watch monkeys play with a slight relief" $ do
      it "should watch a round of monkey in the middle" $ do
        let strategy = KeepAway.slightRelief monkeys
        let result = KeepAway.monkeyRound strategy monkeys
        (M.items <$> Map.lookup 0 result) `shouldBe` Just [20, 23, 27, 26]
        (M.items <$> Map.lookup 1 result) `shouldBe` Just [2080, 25, 167, 207, 401, 1046]
        (M.items <$> Map.lookup 2 result) `shouldBe` Just []
        (M.items <$> Map.lookup 3 result) `shouldBe` Just []

      it "should watch twenty rounds of monkey in the middle" $ do
        let result = KeepAway.playRounds KeepAway.SlightRelief 20 monkeys
        (M.items <$> Map.lookup 0 result) `shouldBe` Just [10, 12, 14, 26, 34]
        (M.items <$> Map.lookup 1 result) `shouldBe` Just [245, 93, 53, 199, 115]
        (M.items <$> Map.lookup 2 result) `shouldBe` Just []
        (M.items <$> Map.lookup 3 result) `shouldBe` Just []

      it "should count how many times a monkey inspects an item" $ do
        let result = KeepAway.playRounds KeepAway.SlightRelief 20 monkeys
        (M.inspected <$> Map.lookup 0 result) `shouldBe` Just 101
        (M.inspected <$> Map.lookup 1 result) `shouldBe` Just 95
        (M.inspected <$> Map.lookup 2 result) `shouldBe` Just 7
        (M.inspected <$> Map.lookup 3 result) `shouldBe` Just 105

      it "should figure out the most active monkeys" $ do
        let result = KeepAway.playRounds KeepAway.SlightRelief 20 monkeys
        KeepAway.business result `shouldBe` 10605

    describe "Watch monkeys play with a no relief at all" $ do
      it "should figure out monkey business" $ do
        let result = KeepAway.playRounds KeepAway.NoRelief 20 monkeys
        (M.inspected <$> Map.lookup 0 result) `shouldBe` Just 99
        (M.inspected <$> Map.lookup 1 result) `shouldBe` Just 97
        (M.inspected <$> Map.lookup 2 result) `shouldBe` Just 8
        (M.inspected <$> Map.lookup 3 result) `shouldBe` Just 103

      it "should figure out monkey business after 1000 rounds" $ do
        let result = KeepAway.playRounds KeepAway.NoRelief 1000 monkeys
        (M.inspected <$> Map.lookup 0 result) `shouldBe` Just 5204
        (M.inspected <$> Map.lookup 1 result) `shouldBe` Just 4792
        (M.inspected <$> Map.lookup 2 result) `shouldBe` Just 199
        (M.inspected <$> Map.lookup 3 result) `shouldBe` Just 5192

      it "should figure out the most active monkeys after 10000 rounds" $ do
        let result = KeepAway.playRounds KeepAway.NoRelief 10000 monkeys
        (M.inspected <$> Map.lookup 0 result) `shouldBe` Just 52166
        (M.inspected <$> Map.lookup 1 result) `shouldBe` Just 47830
        (M.inspected <$> Map.lookup 2 result) `shouldBe` Just 1938
        (M.inspected <$> Map.lookup 3 result) `shouldBe` Just 52013
        KeepAway.business result `shouldBe` 2713310158
