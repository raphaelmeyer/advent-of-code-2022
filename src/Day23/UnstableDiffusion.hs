module Day23.UnstableDiffusion where

import qualified AoC.Puzzle as Puzzle
import qualified Control.Monad.State as S
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Tuple.Extra ((&&&))

solver :: Puzzle.Solver
solver = Puzzle.Solver 23 "🌱 Unstable Diffusion" solve

solve :: String -> Puzzle.Solution
solve = (partOne &&& partTwo) . parseInput . Text.lines . Text.pack

data Input = None deriving (Eq, Show)

type Pos = (Int, Int)

type Elves = Set.Set Pos

type Proposals = [(Pos, Pos)]

data Direction = North | East | South | West deriving (Eq, Show)

data State = State {elfPositions :: Elves, directions :: [Direction]}

-- solution

partOne :: Elves -> String
partOne = show . checkProgress

partTwo :: Elves -> String
partTwo _ = show (0 :: Int)

checkProgress :: Elves -> Int
checkProgress elves = countEmpty . (!! 10) . iterate (S.execState elfRound) $ State elves [North, South, West, East]

countEmpty :: State -> Int
countEmpty s = w * h - Set.size (elfPositions s)
  where
    (xs, ys) = unzip . Set.toList . elfPositions $ s
    w = maximum xs - minimum xs + 1
    h = maximum ys - minimum ys + 1

elfRound :: S.State State ()
elfRound = do
  proposals <- lookAround
  walkSteps proposals
  rotateDirections

lookAround :: S.State State Proposals
lookAround = S.gets (\s -> foldl (decide s) [] (elfPositions s))

walkSteps :: Proposals -> S.State State ()
walkSteps ps = do
  s <- S.get
  let moved = foldl walk (elfPositions s) moves
  S.put (s {elfPositions = moved})
  where
    walk elves move = Set.insert (snd move) . Set.delete (fst move) $ elves
    moves = filter ((`elem` uniques) . snd) ps
    uniques = concat . filter ((== 1) . length) . List.group . List.sort . map snd $ ps

rotateDirections :: S.State State ()
rotateDirections = do
  s <- S.get
  let ds = directions s
  S.put (s {directions = tail ds ++ [head ds]})

decide :: State -> Proposals -> Pos -> Proposals
decide s ps me = if any (`Set.member` elfPositions s) neighbors then proposal else ps
  where
    neighboring = map (add me) offsets
    neighbors = take 8 neighboring
    add (x, y) (i, j) = (x + i, y + j)
    proposal = if Maybe.isJust valid then (me, Maybe.fromJust valid) : ps else ps
    valid = look s (directions s) neighboring

look :: State -> [Direction] -> [Pos] -> Maybe Pos
look _ [] _ = Nothing
look s (d : ds) neighbors = if any (`Set.member` elfPositions s) direction then look s ds neighbors else Just (neighbors !! (offset d + 1))
  where
    direction = take 3 . drop (offset d) $ neighbors

offsets :: [Pos]
offsets = [(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1)]

offset :: Direction -> Int
offset North = 0
offset South = 4
offset East = 2
offset West = 6

-- parse input

parseInput :: [Text.Text] -> Elves
parseInput = foldl parseRow Set.empty . zip [0 ..]

parseRow :: Elves -> (Int, Text.Text) -> Elves
parseRow elves (row, line) = foldl addElf elves . zip (zip [0 ..] (repeat row)) . Text.unpack $ line

addElf :: Elves -> (Pos, Char) -> Elves
addElf elves (pos, '#') = Set.insert pos elves
addElf elves _ = elves
