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

walk :: Area -> Strategy -> Seq.Seq (Pos, Distance) -> Set.Set Pos -> Distance
walk area strategy queue explored = if isGoal strategy (me, h) then dist else walk area strategy queue' explored'
  where
    (me, dist) = Maybe.fromMaybe (error . show $ explored) (Seq.lookup 0 queue)
    h = Maybe.fromJust . atArea area $ me
    adjacent = map (\p -> (p, dist + 1)) . filter (`Set.notMember` explored) . Maybe.mapMaybe (reachable . (\d -> d me)) $ [up, down, left, right]
    reachable n = case atArea area n of
      Just to -> if isReachable strategy (to - h) then Just n else Nothing
      _ -> Nothing
    explored' = foldr (Set.insert . fst) explored adjacent
    queue' = foldl (Seq.|>) (Seq.drop 1 queue) adjacent

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
