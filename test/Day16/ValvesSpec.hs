{-# LANGUAGE OverloadedStrings #-}

module Day16.ValvesSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Day16.Valves as Valves
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB",
    "Valve BB has flow rate=13; tunnels lead to valves CC, AA",
    "Valve CC has flow rate=2; tunnels lead to valves DD, BB",
    "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE",
    "Valve EE has flow rate=3; tunnels lead to valves FF, DD",
    "Valve FF has flow rate=0; tunnels lead to valves EE, GG",
    "Valve GG has flow rate=0; tunnels lead to valves FF, HH",
    "Valve HH has flow rate=22; tunnel leads to valve GG",
    "Valve II has flow rate=0; tunnels lead to valves AA, JJ",
    "Valve JJ has flow rate=21; tunnel leads to valve II"
  ]

spec :: Spec
spec = do
  describe "Proboscidea Volcanium" $ do
    let valves = Valves.parseInput exampleInput
    let graph = Valves.makeGraph valves

    it "should find the most pressure you can release" $ do
      Valves.mostPressure graph `shouldBe` 1651

    it "should identify all functioning valves" $ do
      Valves.vertices graph Map.!? "BB" `shouldBe` Just 13
      Valves.vertices graph Map.!? "HH" `shouldBe` Just 22
      Valves.vertices graph Map.!? "II" `shouldBe` Nothing

    it "should find all shortest paths from any valve to any other" $ do
      Valves.edges graph Map.! ("AA", "EE") `shouldBe` 2
      Valves.edges graph Map.! ("BB", "DD") `shouldBe` 2
      Valves.edges graph Map.! ("BB", "HH") `shouldBe` 6

    it "should train an elephant for help" $ do
      Valves.withElephant graph `shouldBe` 1707
