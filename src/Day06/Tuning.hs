{-# LANGUAGE OverloadedStrings #-}

module Day06.Tuning where

import qualified AoC.Puzzle as Puzzle
import qualified Data.List as List
import qualified Data.Text as Text
import Data.Tuple.Extra ((&&&))

solver :: Puzzle.Solver
solver = Puzzle.Solver 6 "📟 Tuning Trouble" solve

solve :: String -> Puzzle.Solution
solve = (show . packetMarker &&& show . messageMarker) . Text.pack

-- solution

packetMarker :: Text.Text -> Int
packetMarker = startMarker 4

messageMarker :: Text.Text -> Int
messageMarker = startMarker 14

startMarker :: Int -> Text.Text -> Int
startMarker len input = case find' input of
  Just n -> n + len
  Nothing -> -1
  where
    find' = List.findIndex ((== len) . length . List.nub . Text.unpack . Text.take len) . Text.tails
