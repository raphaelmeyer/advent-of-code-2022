module Day24.BlizzardBasin where

import qualified AoC.Puzzle as Puzzle
import qualified Control.Monad.State as S
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import Data.Tuple.Extra ((&&&))

solver :: Puzzle.Solver
solver = Puzzle.Solver 24 "❄️ Blizzard Basin" solve

solve :: String -> Puzzle.Solution
solve = (partOne &&& partTwo) . parseInput . Text.lines . Text.pack

data Direction = MoveUp | MoveDown | MoveLeft | MoveRight deriving (Eq, Show)

type Pos = (Int, Int)

type Blizzard = (Pos, Direction)

type Blizzards = [Blizzard]

data State = State {blizzards :: Blizzards, positions :: [Pos], time :: Int, start :: Pos, goal :: Pos, width :: Int, height :: Int} deriving (Eq, Show)

-- solution

partOne :: State -> String
partOne = show . goForExit

partTwo :: State -> String
partTwo = show . goForSnack

goForExit :: State -> Int
goForExit s = S.evalState takeStep $ s {time = 0}

goForSnack :: State -> Int
goForSnack s = S.evalState takeStep $ back {positions = [start s], start = start s, goal = goal s}
  where
    there = S.execState takeStep $ s {time = 0}
    back = S.execState takeStep $ there {positions = [goal s], start = goal s, goal = start s}

takeStep :: S.State State Int
takeStep = do
  updateTime
  updateBlizzards
  updatePositions
  s <- S.get
  if goal s `elem` positions s
    then return (time s)
    else takeStep

updateBlizzards :: S.State State ()
updateBlizzards = S.modify (\s -> s {blizzards = map (updateBlizzard (width s) (height s)) (blizzards s)})

updatePositions :: S.State State ()
updatePositions = S.modify (\s -> s {positions = foldl (updatePosition s) [] (positions s)})

updateTime :: S.State State ()
updateTime = S.modify (\s -> s {time = time s + 1})

updateBlizzard :: Int -> Int -> Blizzard -> Blizzard
updateBlizzard _ h ((x, y), MoveUp)
  | y == 0 = ((x, h - 1), MoveUp)
  | otherwise = ((x, y - 1), MoveUp)
updateBlizzard _ h ((x, y), MoveDown)
  | y == h - 1 = ((x, 0), MoveDown)
  | otherwise = ((x, y + 1), MoveDown)
updateBlizzard w _ ((x, y), MoveLeft)
  | x == 0 = ((w - 1, y), MoveLeft)
  | otherwise = ((x - 1, y), MoveLeft)
updateBlizzard w _ ((x, y), MoveRight)
  | x == w - 1 = ((0, y), MoveRight)
  | otherwise = ((x + 1, y), MoveRight)

updatePosition :: State -> [Pos] -> Pos -> [Pos]
updatePosition s ps pos = List.nub $ ps ++ reachable
  where
    reachable = filter (\p@(x, y) -> p == start s || p == goal s || (0 <= x && x < w && 0 <= y && y < h)) noBlizzard
    noBlizzard = filter (Maybe.isNothing . (`List.lookup` blizzards s)) adjacent
    adjacent = map (\(i, j) -> (fst pos + i, snd pos + j)) [(0, 0), (-1, 0), (1, 0), (0, -1), (0, 1)]
    w = width s
    h = height s

-- parse input

parseInput :: [Text.Text] -> State
parseInput input = State {blizzards = blizzs, positions = [entry], time = 0, start = entry, goal = exit, width = w, height = h}
  where
    blizzs = foldl parseRow [] . zip [0 ..] . tail . init $ input
    entry = (hole (head input), -1)
    exit = (hole (last input), length input - 2)
    hole = subtract 1 . Maybe.fromJust . Text.findIndex (== '.')
    w = subtract 2 . Text.length . head $ input
    h = length input - 2

parseRow :: Blizzards -> (Int, Text.Text) -> Blizzards
parseRow blizzs (row, line) = foldl addBlizzard blizzs . zip (zip [-1 ..] (repeat row)) . Text.unpack $ line

addBlizzard :: Blizzards -> (Pos, Char) -> Blizzards
addBlizzard blizzs (pos, '>') = (pos, MoveRight) : blizzs
addBlizzard blizzs (pos, '<') = (pos, MoveLeft) : blizzs
addBlizzard blizzs (pos, '^') = (pos, MoveUp) : blizzs
addBlizzard blizzs (pos, 'v') = (pos, MoveDown) : blizzs
addBlizzard blizzs _ = blizzs
