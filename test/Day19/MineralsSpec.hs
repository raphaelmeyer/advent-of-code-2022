{-# LANGUAGE OverloadedStrings #-}

module Day19.MineralsSpec where

import qualified Control.Monad.State as S
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

      M.blueprintId blueprint `shouldBe` 2
      Map.lookup M.Ore (M.buildCosts blueprint) `shouldBe` Just [M.Cost M.Ore 2]
      Map.lookup M.Geode (M.buildCosts blueprint) `shouldBe` Just [M.Cost M.Ore 3, M.Cost M.Obsidian 12]

    it "should check whether a robot can be built" $ do
      let blueprint = input !! 1
      let config = M.configuration blueprint
      let s = M.State {M.getMinerals = Map.empty, M.getRobots = Map.empty, M.getTime = 24}

      M.Ore `shouldNotSatisfy` M.buildable config (s {M.getRobots = Map.empty})
      M.Ore `shouldSatisfy` M.buildable config (s {M.getRobots = Map.singleton M.Ore 1})

      M.Clay `shouldNotSatisfy` M.buildable config (s {M.getRobots = Map.empty})
      M.Clay `shouldSatisfy` M.buildable config (s {M.getRobots = Map.singleton M.Ore 1})

      M.Obsidian `shouldNotSatisfy` M.buildable config (s {M.getRobots = Map.empty})
      M.Obsidian `shouldNotSatisfy` M.buildable config (s {M.getRobots = Map.singleton M.Ore 1})
      M.Obsidian `shouldNotSatisfy` M.buildable config (s {M.getRobots = Map.singleton M.Clay 1})
      M.Obsidian `shouldSatisfy` M.buildable config (s {M.getRobots = Map.fromList [(M.Ore, 1), (M.Clay, 1)]})

      M.Geode `shouldNotSatisfy` M.buildable config (s {M.getRobots = Map.fromList [(M.Ore, 1), (M.Clay, 1)]})
      M.Geode `shouldSatisfy` M.buildable config (s {M.getRobots = Map.fromList [(M.Ore, 1), (M.Obsidian, 1)]})

    it "should calculate the additionaly required minerals" $ do
      M.requiredMinerals [M.Cost M.Ore 2, M.Cost M.Clay 3] (Map.fromList [(M.Ore, 1), (M.Clay, 1)]) `shouldMatchList` [(M.Ore, 1), (M.Clay, 2)]
      M.requiredMinerals [M.Cost M.Ore 2, M.Cost M.Clay 3] (Map.fromList [(M.Ore, 2), (M.Clay, 3)]) `shouldMatchList` []
      M.requiredMinerals [M.Cost M.Ore 2, M.Cost M.Clay 3] (Map.fromList [(M.Ore, 4)]) `shouldMatchList` [(M.Clay, 3)]
      M.requiredMinerals [M.Cost M.Ore 4, M.Cost M.Obsidian 5] Map.empty `shouldMatchList` [(M.Ore, 4), (M.Obsidian, 5)]

    it "should calculate the limit of any robot required" $ do
      Map.toList (M.robotLimits (input !! 1)) `shouldBe` [(M.Ore, 3), (M.Clay, 8), (M.Obsidian, 12)]

    describe "Build robots and mine minerals" $ do
      let blueprint = input !! 1
      let config = M.configuration blueprint

      it "should build robots if resources are already available" $ do
        let s = S.execState (M.mineAndBuild config M.Obsidian) (M.State (Map.fromList [(M.Ore, 3), (M.Clay, 10)]) (Map.fromList [(M.Obsidian, 2)]) 24)

        (Map.toList . M.getMinerals $ s) `shouldMatchList` [(M.Ore, 0), (M.Clay, 2), (M.Obsidian, 2)]
        (Map.toList . M.getRobots $ s) `shouldMatchList` [(M.Obsidian, 3)]
        M.getTime s `shouldBe` 23

      it "should mine until enough resources available and then build robots" $ do
        let s = S.execState (M.mineAndBuild config M.Geode) (M.State (Map.fromList [(M.Obsidian, 3)]) (Map.fromList [(M.Ore, 1), (M.Obsidian, 2)]) 24)

        (Map.toList . M.getMinerals $ s) `shouldMatchList` [(M.Ore, 3), (M.Obsidian, 3)]
        (Map.toList . M.getRobots $ s) `shouldMatchList` [(M.Ore, 1), (M.Obsidian, 2), (M.Geode, 1)]
        M.getTime s `shouldBe` 18

      it "should not build a robot when not enough time" $ do
        let s = S.execState (M.mineAndBuild config M.Obsidian) (M.State Map.empty (Map.fromList [(M.Ore, 3), (M.Clay, 1), (M.Obsidian, 2)]) 5)

        (Map.toList . M.getMinerals $ s) `shouldMatchList` [(M.Ore, 15), (M.Clay, 5), (M.Obsidian, 10)]
        (Map.toList . M.getRobots $ s) `shouldMatchList` [(M.Ore, 3), (M.Clay, 1), (M.Obsidian, 2)]
        M.getTime s `shouldBe` 0

    describe "Collect geode and calculate quality levels" $ do
      it "should try to collect as much geode as possible" $ do
        M.investigateBlueprint (input !! 0) 24 `shouldBe` 9
        M.investigateBlueprint (input !! 1) 24 `shouldBe` 12

      it "should calculate the sum of quality levels" $ do
        M.qualityLevelSum input `shouldBe` 33
