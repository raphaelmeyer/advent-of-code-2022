module Day03.Rucksack where

import qualified AoC.Puzzle as Puzzle
import qualified Data.Char as Char
import qualified Data.List.Split as Split (chunksOf)
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Text as Text
import Data.Tuple.Extra ((&&&))

solver :: Puzzle.Solver
solver = Puzzle.Solver 3 "🎒 Rucksack Reorganization" solve

solve :: String -> Puzzle.Solution
solve = (partOne &&& partTwo) . Text.lines . Text.pack

-- solution

partOne :: [Text.Text] -> String
partOne = show . prioritySum . parseInput

partTwo :: [Text.Text] -> String
partTwo = show . badgePrioSum

type Rucksack = (Text.Text, Text.Text)

duplicateItems :: [Rucksack] -> [Char]
duplicateItems = mapMaybe duplicateItem

duplicateItem :: Rucksack -> Maybe Char
duplicateItem (one, two) = Text.find (`Text.elem` two) one

prioritySum :: [Rucksack] -> Int
prioritySum = sum . map priority . duplicateItems

badgePrioSum :: [Text.Text] -> Int
badgePrioSum = sum . map priority . findBadges

priority :: Char -> Int
priority c
  | Char.isUpper c = Char.ord c - Char.ord 'A' + 27
  | Char.isLower c = Char.ord c - Char.ord 'a' + 1
  | otherwise = 0

findBadges :: [Text.Text] -> [Char]
findBadges = mapMaybe findBadge . Split.chunksOf 3

findBadge :: [Text.Text] -> Maybe Char
findBadge (r : rs) = listToMaybe . Text.unpack . foldl search r $ rs
  where
    search candidates items = Text.filter (`Text.elem` items) candidates
findBadge _ = Nothing

-- parse input

parseInput :: [Text.Text] -> [Rucksack]
parseInput = map split
  where
    split items = Text.splitAt (middle items) items
    middle = (`div` 2) . Text.length
