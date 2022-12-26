module Day20.GPS where

import qualified AoC.Puzzle as Puzzle
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Read as Read
import Data.Tuple.Extra ((&&&))
import qualified Data.Vector as Vector

solver :: Puzzle.Solver
solver = Puzzle.Solver 20 "🔒 Grove Positioning System" solve

solve :: String -> Puzzle.Solution
solve = (partOne &&& partTwo) . parseInput . Text.lines . Text.pack

-- solution

partOne :: [Int] -> String
partOne = show . mixSum 1 1

partTwo :: [Int] -> String
partTwo = show . mixSum 811589153 10

mixSum :: Int -> Int -> [Int] -> Int
mixSum key cycles file = sum . map (nthElement . mixNTimes key cycles $ file) $ [1000, 2000, 3000]

type Values = Vector.Vector Int

mixNTimes :: Int -> Int -> [Int] -> [Int]
mixNTimes key n file = toValues . (!! n) . iterate (mix mods positions) $ positions
  where
    values = Vector.fromList . map (* key) $ file
    mods = Vector.map (`mod` (Vector.length values - 1)) values
    positions = [0 .. (Vector.length values - 1)]
    toValues = map (values Vector.!)

nthElement :: [Int] -> Int -> Int
nthElement file n = case List.elemIndex 0 file of
  Just i -> file !! mod (i + n) (length file)
  Nothing -> undefined

mix :: Values -> [Int] -> [Int] -> [Int]
mix mods positions file = foldl (mixNumber mods) file positions

mixNumber :: Values -> [Int] -> Int -> [Int]
mixNumber values file n = case List.elemIndex n file of
  Just i -> move values i n file
  Nothing -> undefined

move :: Values -> Int -> Int -> [Int] -> [Int]
move values i n file = if value == 0 then file else inserted
  where
    value = values Vector.! n
    dest = normalize (Vector.length values - 1) value
    rotated = (\(a, b) -> tail b ++ a) . List.splitAt i $ file
    inserted = (\(a, b) -> a ++ [n] ++ b) . List.splitAt dest $ rotated

normalize :: Int -> Int -> Int
normalize len n
  | n < 0 = n + len
  | n > len = n - len
  | otherwise = n

-- parse input

parseInput :: [Text.Text] -> [Int]
parseInput = map fst . Either.rights . map (Read.signed Read.decimal)
