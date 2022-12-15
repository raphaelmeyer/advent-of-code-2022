module Day12.HillClimbing where

import qualified AoC.Puzzle as Puzzle
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Tuple.Extra ((&&&))
import qualified Data.Vector as Vector

solver :: Puzzle.Solver
solver = Puzzle.Solver 12 "⛰ Hill Climbing Algorithm" solve

solve :: String -> Puzzle.Solution
solve = (partOne &&& partTwo) . parseInput . Text.lines . Text.pack

-- solution

type Pos = (Int, Int)

type HeightMap = Vector.Vector (Vector.Vector Int)

data Area = Area {areaMap :: HeightMap, start :: Pos, goal :: Pos}

partOne :: Area -> String
partOne = show . shortestPath climb

partTwo :: Area -> String
partTwo = show . shortestPath hike

type Height = Int

type Distance = Int

data Strategy = Strategy {from :: Pos, isGoal :: (Pos, Height) -> Bool, isReachable :: Int -> Bool}

shortestPath :: (Area -> Strategy) -> Area -> Distance
shortestPath strategy area = walk area selected (Seq.singleton (f, 0)) (Set.singleton f)
  where
    selected = strategy area
    f = from selected

climb :: Area -> Strategy
climb area = Strategy (start area) (\(p, _) -> p == goal area) (<= 1)

hike :: Area -> Strategy
hike area = Strategy (goal area) (\(_, to) -> to == 0) (>= -1)

atArea :: Area -> Pos -> Maybe Int
atArea area (i, j) = case (Vector.!? j) . areaMap $ area of
  Just row -> row Vector.!? i
  Nothing -> Nothing

up :: Pos -> Pos
up (i, j) = (i, j - 1)

down :: Pos -> Pos
down (i, j) = (i, j + 1)

left :: Pos -> Pos
left (i, j) = (i - 1, j)

right :: Pos -> Pos
right (i, j) = (i + 1, j)

type Queue = Seq.Seq (Pos, Distance)

type Explored = Set.Set Pos

walk :: Area -> Strategy -> Queue -> Explored -> Distance
walk area strategy queue explored = if isGoal strategy (me, height) then dist else walk area strategy queue' explored'
  where
    (me, dist) = Maybe.fromJust (Seq.lookup 0 queue)
    height = Maybe.fromJust . atArea area $ me
    explored' = foldr (Set.insert . fst) explored visited
    queue' = foldl (Seq.|>) (Seq.drop 1 queue) visited
    visited = visit . unexplored $ adjacent
      where
        adjacent = Maybe.mapMaybe (reachable . (\go -> go me)) [up, down, left, right]
        reachable target = case atArea area target of
          Just to -> if isReachable strategy (to - height) then Just target else Nothing
          _ -> Nothing
        unexplored = filter (`Set.notMember` explored)
        visit = map (`tuple` (dist + 1))

tuple :: a -> b -> (a, b)
tuple a b = (a, b)

-- parse input

parseInput :: [Text.Text] -> Area
parseInput = parseGrid . makeGrid

makeGrid :: [Text.Text] -> Vector.Vector (Vector.Vector Char)
makeGrid = Vector.fromList . map (Vector.fromList . Text.unpack)

parseGrid :: Vector.Vector (Vector.Vector Char) -> Area
parseGrid input = area {areaMap = Vector.fromList grid}
  where
    (grid, area) = Vector.ifoldr parseRow ([Vector.empty], Area Vector.empty (0, 0) (0, 0)) input

parseRow :: Int -> Vector.Vector Char -> ([Vector.Vector Int], Area) -> ([Vector.Vector Int], Area)
parseRow j line (rows, area) = (Vector.fromList row : rows, area')
  where
    (row, area') = Vector.ifoldr parseHeight ([], area) line
      where
        parseHeight i c (hs, area'') = case c of
          'S' -> (0 : hs, area'' {start = (i, j)})
          'E' -> (25 : hs, area'' {goal = (i, j)})
          h -> ((Char.ord h - Char.ord 'a') : hs, area'')
