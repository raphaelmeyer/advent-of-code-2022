{-# LANGUAGE BinaryLiterals #-}

module Day17.PyroclasticFlow where

import qualified AoC.Puzzle as Puzzle
import qualified Data.Bits as Bits
import qualified Data.List as List
import qualified Data.Text as Text
import Data.Tuple.Extra ((&&&))
import qualified Data.Vector as Vector

solver :: Puzzle.Solver
solver = Puzzle.Solver 17 "🟥 Pyroclastic Flow" solve

solve :: String -> Puzzle.Solution
solve = (partOne &&& partTwo) . parseInput . Text.pack

-- solution

partOne :: [Jet] -> String
partOne = show . length . simulate 2022

partTwo :: [Jet] -> String
partTwo = show . simulate2 1000000000000

data Jet = PushLeft | PushRight deriving (Eq, Show)

type Jets = [Jet]

type Rock = [Int]

type Tower = [Int]

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

-- part one

simulate :: Int -> [Jet] -> Tower
simulate n jets = dropWhile (== 0) . fst . foldl simulateRock ([], cycle jets) $ (take n . cycle $ rocks)

simulateRock :: (Tower, Jets) -> Rock -> (Tower, Jets)
simulateRock (ts, jets) = simulateStep jets ([], equalize ts)

simulateStep :: Jets -> ([Int], [Int]) -> Rock -> (Tower, Jets)
simulateStep jets (top, []) rock = (addRock (top, []) pushed, tail jets)
  where
    pushed = tryPush top rock (head jets)
simulateStep jets (top, b : bottom) rock =
  if rockFits (b : top) pushed
    then simulateStep (tail jets) (b : top, bottom) pushed
    else (addRock (top, b : bottom) pushed, tail jets)
  where
    pushed = tryPush top rock (head jets)

addRock :: ([Int], [Int]) -> Rock -> Tower
addRock (top, bottom) [] = reverse top ++ bottom
addRock ([], bottom) rock = reverse rock ++ bottom
addRock (t : top, bottom) (r : rock) = addRock (top, (Bits..|.) t r : bottom) rock

equalize :: Tower -> Tower
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

-- part two

data State = State
  { tower :: Tower,
    numRocks :: Int,
    jetIndex :: Int,
    jetList :: Vector.Vector Jet,
    rockIndex :: Int,
    rockList :: Vector.Vector Rock,
    patterns :: [((Tower, Int), Int)],
    cycleLen :: Int
  }
  deriving (Eq, Show)

simulate2 :: Int -> [Jet] -> Int
simulate2 n jets = (length . tower $ untilTop) + cycleHeight * notSimulated
  where
    initial = State [] 0 0 (Vector.fromList jets) 0 (Vector.fromList rocks) [] 0
    firstCycle = simulate2FindCycle initial
    secondCycle = simulate2FindCycle (firstCycle {cycleLen = 0})
    cycleHeight = (length . tower $ secondCycle) - (length . tower $ firstCycle)
    remainingRocks = n - numRocks secondCycle
    notSimulated = div remainingRocks (cycleLen secondCycle)
    rocksRest = mod remainingRocks (cycleLen secondCycle)
    untilTop = simulate2NSteps rocksRest secondCycle

simulate2FindCycle :: State -> State
simulate2FindCycle s
  | cycleLen simulated > 0 = trimTower simulated
  | otherwise = simulate2FindCycle simulated
  where
    rock = rockList s Vector.! rockIndex s
    simulated = updateRockIndex . simulate2Step rock (s {tower = equalize (tower s)}) $ []

simulate2NSteps :: Int -> State -> State
simulate2NSteps n s
  | n == 0 = trimTower s
  | otherwise = simulate2NSteps (n - 1) simulated
  where
    rock = rockList s Vector.! rockIndex s
    simulated = updateRockIndex . simulate2Step rock (s {tower = equalize (tower s)}) $ []

simulate2Step :: Rock -> State -> [Int] -> State
simulate2Step rock s top = dropRock pushed top . updateJetIndex $ s
  where
    jet = jetList s Vector.! jetIndex s
    pushed = tryPush top rock jet

dropRock :: Rock -> [Int] -> State -> State
dropRock rock top s
  | null (tower s) = s {tower = addRock (top, []) rock, numRocks = numRocks s + 1}
  | otherwise =
      if rockFits (t : top) rock
        then simulate2Step rock (s {tower = bottom}) (t : top)
        else s {tower = addRock (top, tower s) rock, numRocks = numRocks s + 1}
  where
    t = head (tower s)
    bottom = drop 1 (tower s)

updateJetIndex :: State -> State
updateJetIndex s =
  if new < Vector.length (jetList s)
    then s {jetIndex = new}
    else searchCycle (s {jetIndex = 0})
  where
    new = jetIndex s + 1

updateRockIndex :: State -> State
updateRockIndex s = s {rockIndex = mod (rockIndex s + 1) (Vector.length (rockList s))}

searchCycle :: State -> State
searchCycle s = searchPattern s top
  where
    top = take 50 (tower s)

searchPattern :: State -> Tower -> State
searchPattern s ts = case List.find (compare' (ts, rockIndex s)) (patterns s) of
  Just p -> s {cycleLen = numRocks s - snd p, patterns = [pat]}
  Nothing -> s {patterns = pat : patterns s}
  where
    compare' a (b, _) = a == b
    pat = ((ts, rockIndex s), numRocks s)

trimTower :: State -> State
trimTower s = s {tower = dropWhile (== 0b0000000) (tower s)}

-- parse input

parseInput :: Text.Text -> [Jet]
parseInput = Text.foldr toJet []
  where
    toJet c js = case c of
      '<' -> PushLeft : js
      '>' -> PushRight : js
      _ -> js
