module Day01.Calories where

import qualified AoC.Puzzle as Puzzle
import qualified Data.Either as Either (rights)
import qualified Data.List as List (sort)
import qualified Data.List.Split as Split
import qualified Data.Text as Text
import qualified Data.Text.Read as Read (decimal)
import Data.Tuple.Extra ((&&&))
import qualified Data.Tuple.Extra as Tuple

solver :: Puzzle.Solver
solver = Puzzle.Solver 1 "🍫 Calorie Counting" solve

solve :: String -> Puzzle.Solution
solve = Tuple.both show . (mostCalories &&& topThreeSum) . parseInput . Text.lines . Text.pack

-- solution

mostCalories :: [[Int]] -> Int
mostCalories = maximum . map sum

topThreeSum :: [[Int]] -> Int
topThreeSum = sum . take 3 . reverse . List.sort . map sum

-- parse input

parseInput :: [Text.Text] -> [[Int]]
parseInput = map parseCalories . Split.splitWhen Text.null

parseCalories :: [Text.Text] -> [Int]
parseCalories = map fst . Either.rights . map Read.decimal
