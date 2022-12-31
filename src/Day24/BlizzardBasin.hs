module Day24.BlizzardBasin where

import qualified AoC.Puzzle as Puzzle
import qualified Control.Monad.State as S
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Tuple.Extra ((&&&))
import qualified Data.Vector as Vector

solver :: Puzzle.Solver
solver = Puzzle.Solver 24 "❄️ Blizzard Basin" solve

solve :: String -> Puzzle.Solution
solve = (partOne &&& partTwo) . parseInput . Text.lines . Text.pack

data Blizzard = None | MoveUp | MoveDown | MoveLeft | MoveRight deriving (Eq, Show)

type Pos = (Int, Int)

type Blizzards = Vector.Vector (Vector.Vector Blizzard)

data State = State {blizzards :: Blizzards, positions :: Set.Set Pos, time :: Int, start :: Pos, goal :: Pos, width :: Int, height :: Int} deriving (Eq, Show)

-- solution

partOne :: State -> String
partOne = show . goForExit

partTwo :: State -> String
partTwo = show . goForSnack

goForExit :: State -> Int
goForExit s = S.evalState takeStep $ s {time = 0}

goForSnack :: State -> Int
goForSnack s = S.evalState takeStep $ back {positions = Set.singleton (start s), start = start s, goal = goal s}
  where
    there = S.execState takeStep $ s {time = 0}
    back = S.execState takeStep $ there {positions = Set.singleton (goal s), start = goal s, goal = start s}

takeStep :: S.State State Int
takeStep = do
  updateTime
  updatePositions
  s <- S.get
  if goal s `elem` positions s
    then return (time s)
    else takeStep

updatePositions :: S.State State ()
updatePositions = S.modify (\s -> s {positions = foldl (updatePosition s) Set.empty (positions s)})

updateTime :: S.State State ()
updateTime = S.modify (\s -> s {time = time s + 1})

updatePosition :: State -> Set.Set Pos -> Pos -> Set.Set Pos
updatePosition s ps pos = foldr Set.insert ps reachable
  where
    reachable = filter (\p@(x, y) -> p == start s || p == goal s || (0 <= x && x < w && 0 <= y && y < h)) noBlizzard
    noBlizzard = filter (checkEmpty s) adjacent
    adjacent = map (\(i, j) -> (fst pos + i, snd pos + j)) [(0, 0), (-1, 0), (1, 0), (0, -1), (0, 1)]
    w = width s
    h = height s

checkEmpty :: State -> Pos -> Bool
checkEmpty s (x, y) = and [right, left, up, down]
  where
    right = blizzs `at` (mod (x - m) w, y) /= MoveRight
    left = blizzs `at` (mod (x + m) w, y) /= MoveLeft
    down = blizzs `at` (x, mod (y - m) h) /= MoveDown
    up = blizzs `at` (x, mod (y + m) h) /= MoveUp
    blizzs = blizzards s
    w = width s
    h = height s
    m = time s

at :: Blizzards -> Pos -> Blizzard
at blizzs (x, y) = Maybe.fromMaybe None (blizzs Vector.!? y >>= (Vector.!? x))

-- parse input

parseInput :: [Text.Text] -> State
parseInput input = State {blizzards = blizzs, positions = Set.singleton entry, time = 0, start = entry, goal = exit, width = w, height = h}
  where
    blizzs = Vector.fromList . map parseRow . tail . init $ input
    entry = (hole (head input), -1)
    exit = (hole (last input), length input - 2)
    hole = subtract 1 . Maybe.fromJust . Text.findIndex (== '.')
    w = subtract 2 . Text.length . head $ input
    h = length input - 2

parseRow :: Text.Text -> Vector.Vector Blizzard
parseRow = Vector.fromList . map parseBlizzard . tail . init . Text.unpack

parseBlizzard :: Char -> Blizzard
parseBlizzard '>' = MoveRight
parseBlizzard '<' = MoveLeft
parseBlizzard '^' = MoveUp
parseBlizzard 'v' = MoveDown
parseBlizzard _ = None
