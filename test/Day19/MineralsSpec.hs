{-# LANGUAGE OverloadedStrings #-}

module Day19.MineralsSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Day19.Minerals as M
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.",
    "Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian."
  ]

spec :: Spec
spec = do
  describe "Not Enough Minerals" $ do
    let input = M.parseInput exampleInput

    it "should parse the input" $ do
      length input `shouldBe` 2
      let blueprint = input !! 1

      M.blueprint blueprint `shouldBe` 2
      Map.lookup M.Ore (M.costs blueprint) `shouldBe` Just [M.Cost M.Ore 2]
      Map.lookup M.Geode (M.costs blueprint) `shouldBe` Just [M.Cost M.Ore 3, M.Cost M.Obsidian 12]

    it "should determine what robots can be build with the current resources" $ do
      let state = (\s -> s {M.getMinerals = Map.fromList [(M.Obsidian, 10), (M.Clay, 14), (M.Ore, 3)]}) . M.initialState $ head input
      state `shouldSatisfy` M.canBuildRobot M.Geode
      state `shouldSatisfy` M.canBuildRobot M.Obsidian
      state `shouldSatisfy` M.canBuildRobot M.Clay
      state `shouldNotSatisfy` M.canBuildRobot M.Ore

      let state2 = (\s -> s {M.getMinerals = Map.fromList [(M.Obsidian, 6), (M.Clay, 5), (M.Ore, 4)]}) . M.initialState $ head input
      state2 `shouldNotSatisfy` M.canBuildRobot M.Geode
      state2 `shouldNotSatisfy` M.canBuildRobot M.Obsidian
      state2 `shouldSatisfy` M.canBuildRobot M.Clay
      state2 `shouldSatisfy` M.canBuildRobot M.Ore

    it "should build robots" $ do
      let state = (\s -> s {M.getMinerals = Map.fromList [(M.Obsidian, 10), (M.Clay, 10), (M.Ore, 10)], M.getRobots = Map.fromList [(M.Clay, 1), (M.Ore, 2)]}) . M.initialState $ head input
      let build r = M.launchRobot r . M.buildRobot r
      let built = build M.Ore . build M.Geode $ state

      Map.toList (M.getRobots built) `shouldMatchList` [(M.Geode, 1), (M.Clay, 1), (M.Ore, 3)]
      Map.toList (M.getMinerals built) `shouldMatchList` [(M.Obsidian, 3), (M.Clay, 10), (M.Ore, 4)]

    it "should collect minerals" $ do
      let state = (\s -> s {M.getMinerals = Map.fromList [(M.Geode, 3), (M.Clay, 3)], M.getRobots = Map.fromList [(M.Geode, 2), (M.Obsidian, 1)]}) . M.initialState $ head input
      let collected = M.collectMinerals state

      Map.toList (M.getMinerals collected) `shouldMatchList` [(M.Geode, 5), (M.Obsidian, 1), (M.Clay, 3)]

    it "should calculate the limit of any robot required" $ do
      let state = M.initialState (input !! 1)
      Map.toList (M.maxRobots state) `shouldBe` [(M.Ore, 3), (M.Clay, 8), (M.Obsidian, 12)]

    it "should try to collect as much geode as possible" $ do
      M.investigateBlueprint (input !! 0) `shouldBe` 9
      M.investigateBlueprint (input !! 1) `shouldBe` 12

    it "should calculate the sum of quality levels" $ do
      M.qualityLevelSum input `shouldBe` 33
