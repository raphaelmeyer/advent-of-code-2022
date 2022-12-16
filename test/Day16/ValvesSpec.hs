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

    it "should create reduced graph of tubes and pipes" $ do
      let graph = Valves.makeGraph valves

      Map.size (Valves.vertices graph) `shouldBe` 7
      Map.size (Valves.edges graph) `shouldBe` 7

      Map.lookup "AA" (Valves.vertices graph) `shouldBe` Just 0
      Map.lookup "HH" (Valves.vertices graph) `shouldBe` Just 22
      Map.lookup "FF" (Valves.vertices graph) `shouldBe` Nothing

      Map.lookup ("AA", "JJ") (Valves.edges graph) `shouldBe` Just 2
      Map.lookup ("EE", "HH") (Valves.edges graph) `shouldBe` Just 3
      Map.lookup ("AA", "II") (Valves.edges graph) `shouldBe` Nothing

    it "should find the most pressure you can release" $ do
      Valves.mostPressure valves `shouldBe` 1651

    it "should find all shortest paths from any valve to any other" $ do
      let graph = Valves.makeGraph valves
      let paths = Valves.shortestPath graph "BB"

      (Map.!) paths "DD" `shouldBe` 2
      (Map.!) paths "HH" `shouldBe` 6
