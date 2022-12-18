{-# LANGUAGE BinaryLiterals #-}

module Day17.PyroclasticFlow where

import qualified AoC.Puzzle as Puzzle
import qualified Data.Bits as Bits
import qualified Data.Text as Text
import Data.Tuple.Extra ((&&&))

solver :: Puzzle.Solver
solver = Puzzle.Solver 17 "🟥 Pyroclastic Flow" solve

solve :: String -> Puzzle.Solution
solve = (partOne &&& partTwo) . parseInput . Text.pack

-- solution

partOne :: [Jet] -> String
partOne = show . tallness . simulate 2022

partTwo :: [Jet] -> String
partTwo _ = show (0 :: Int)

data Jet = PushLeft | PushRight deriving (Eq, Show)

type Jets = [Jet]

type Rock = [Int]

rockTypes :: [Rock]
rockTypes =
  [ [0b0011110],
    [0b0001000, 0b0011100, 0b0001000],
    [0b0000100, 0b0000100, 0b0011100],
    [0b0010000, 0b0010000, 0b0010000, 0b0010000],
    [0b0011000, 0b0011000]
  ]

-- rock falling is simulated by un-stacking the tower and then comparing
-- a falling rock with the un-stacked (and therefore reversed) lines. So
-- lets put the rocks upside down.
rocks :: [Rock]
rocks = map reverse rockTypes

tallness :: [Int] -> Int
tallness = length

simulate :: Int -> [Jet] -> [Int]
simulate n jets = dropWhile (== 0) . fst . foldl simulateRock ([], cycle jets) $ (take n . cycle $ rocks)

simulateRock :: ([Int], Jets) -> Rock -> ([Int], Jets)
simulateRock (ts, jets) = simulateStep jets ([], equalize ts)

simulateStep :: Jets -> ([Int], [Int]) -> Rock -> ([Int], Jets)
simulateStep jets (top, []) rock = (addRock (top, []) pushed, tail jets)
  where
    pushed = tryPush top rock (head jets)
simulateStep jets (top, b : bottom) rock =
  if rockFits (b : top) pushed
    then simulateStep (tail jets) (b : top, bottom) pushed
    else (addRock (top, b : bottom) pushed, tail jets)
  where
    pushed = tryPush top rock (head jets)

addRock :: ([Int], [Int]) -> Rock -> [Int]
addRock (top, bottom) [] = reverse top ++ bottom
addRock ([], bottom) rock = reverse rock ++ bottom
addRock (t : top, bottom) (r : rock) = addRock (top, (Bits..|.) t r : bottom) rock

equalize :: [Int] -> [Int]
equalize ts
  | emptyLines > 3 = drop (emptyLines - 3) ts
  | emptyLines < 3 = replicate (3 - emptyLines) 0 ++ ts
  | otherwise = ts
  where
    emptyLines = length . takeWhile (== 0) $ ts

tryPush :: [Int] -> Rock -> Jet -> Rock
tryPush ts rock jet = if rockFits ts pushed then pushed else rock
  where
    pushed = doPush rock jet

doPush :: Rock -> Jet -> Rock
doPush rock PushLeft = if any (`Bits.testBit` 6) rock then rock else map (`Bits.shiftL` 1) rock
doPush rock PushRight = if any (`Bits.testBit` 0) rock then rock else map (`Bits.shiftR` 1) rock

rockFits :: [Int] -> Rock -> Bool
rockFits ts rock = all (\(t, r) -> (Bits..&.) t r == 0) (zip ts rock)

printTower :: [Int] -> Text.Text
printTower = Text.unlines . map printRow
  where
    printRow row = Text.pack . map (\b -> if Bits.testBit row b then '#' else '.') $ [6, 5 .. 0]

-- parse input

parseInput :: Text.Text -> [Jet]
parseInput = Text.foldr toJet []
  where
    toJet c js = case c of
      '<' -> PushLeft : js
      '>' -> PushRight : js
      _ -> js
