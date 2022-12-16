{-# LANGUAGE OverloadedStrings #-}

module Day16.Valves where

import qualified AoC.Puzzle as Puzzle
import Control.Applicative (Alternative ((<|>)))
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Tuple.Extra ((&&&))
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as C (letterChar, space, string)
import qualified Text.Megaparsec.Char.Lexer as L

solver :: Puzzle.Solver
solver = Puzzle.Solver 16 "🐘 Proboscidea Volcanium 🌋" solve

solve :: String -> Puzzle.Solution
solve = (partOne &&& partTwo) . parseInput . Text.lines . Text.pack

-- solution

partOne :: [Valve] -> String
partOne = show . mostPressure

partTwo :: [Valve] -> String
partTwo _ = show (0 :: Int)

data Valve = Valve {label :: Text.Text, rate :: Int, tunnels :: [Text.Text]} deriving (Eq, Show)

type Vertex = Text.Text

type Edge = (Text.Text, Text.Text)

type Vertices = Map.Map Vertex Int

type Edges = Map.Map Edge Int

data Graph = Graph {vertices :: Vertices, edges :: Edges} deriving (Eq, Show)

makeGraph :: [Valve] -> Graph
makeGraph = contractGraph . makeRaw

mostPressure :: [Valve] -> Int
mostPressure valves = followTunnels g ps (SearchState "AA" 30 (Set.singleton "AA"))
  where
    g = makeGraph valves
    ps = allShortestPaths g

type Visited = Set.Set Text.Text

data SearchState = SearchState {current :: Vertex, time :: Int, visisted :: Visited}

openValves :: Graph -> ShortestPaths -> SearchState -> Int
openValves g ps s
  | time s <= 0 = 0
  | Set.size (visisted s) == Map.size (vertices g) = 0
  | otherwise = flow + followTunnels g ps s'
  where
    s' = openValve s
    flow = time s' * (Map.!) (vertices g) (current s')

openValve :: SearchState -> SearchState
openValve s = s {time = time s - 1, visisted = Set.insert (current s) (visisted s)}

followTunnels :: Graph -> ShortestPaths -> SearchState -> Int
followTunnels g ps s = maximum . foldl (followTunnel g ps s) [0] $ unexplored
  where
    unexplored = filter (`Set.notMember` visisted s) (Map.keys . vertices $ g)

followTunnel :: Graph -> ShortestPaths -> SearchState -> [Int] -> Vertex -> [Int]
followTunnel g ps s flows v = openValves g ps (s {current = v, time = time'}) : flows
  where
    time' = time s - ((ps Map.! current s) Map.! v)

type Distances = Map.Map Vertex Int

type Queue = Set.Set Vertex

data PathState = PathState {queue :: Queue, distances :: Distances} deriving (Eq, Show)

type ShortestPaths = Map.Map Vertex Distances

allShortestPaths :: Graph -> ShortestPaths
allShortestPaths g = foldl (\paths v -> Map.insert v (shortestPath g v) paths) Map.empty (Map.keys . vertices $ g)

shortestPath :: Graph -> Vertex -> Distances
shortestPath g source =
  distances . shortestPathDequeue g $
    PathState queue' distances'
  where
    vertices' = Map.keys . vertices $ g
    queue' = Set.fromList vertices'
    distances' =
      Map.insert source 0 . Map.fromList $
        zip vertices' (repeat (maxBound :: Int))

shortestPathDequeue :: Graph -> PathState -> PathState
shortestPathDequeue g s = if Set.null (queue s') then s' else shortestPathDequeue g s'
  where
    u = minDistance s
    removed = s {queue = Set.delete u (queue s)}
    vs = neighbors g u removed
    s' = foldl (shortestPathStep g u) removed vs

neighbors :: Graph -> Vertex -> PathState -> [Vertex]
neighbors g u s = foldl isNeighbor [] (queue s)
  where
    isNeighbor ns v =
      if Map.member (edge u v) (edges g)
        then v : ns
        else ns

shortestPathStep :: Graph -> Vertex -> PathState -> Vertex -> PathState
shortestPathStep g u s v =
  if path < (Map.!) (distances s) v
    then s {distances = Map.insert v path (distances s)}
    else s
  where
    path = (Map.!) (distances s) u + (Map.!) (edges g) (edge u v)

minDistance :: PathState -> Vertex
minDistance s =
  fst
    . List.minimumBy (\a b -> compare (snd a) (snd b))
    . Map.toList
    . Map.filterWithKey (\v _ -> Set.member v (queue s))
    $ distances s

-- process input

makeRaw :: [Valve] -> Graph
makeRaw = foldl addValve (Graph Map.empty Map.empty)

addValve :: Graph -> Valve -> Graph
addValve (Graph vs es) (Valve l r ts) = Graph vs' es'
  where
    vs' = Map.insert l r vs
    es' = foldl (addTunnel l) es ts

addTunnel :: Text.Text -> Edges -> Text.Text -> Edges
addTunnel from es to = Map.insert (edge from to) 1 es

edge :: Text.Text -> Text.Text -> Edge
edge a b = (min a b, max a b)

contractGraph :: Graph -> Graph
contractGraph g = foldl removeVertex g . filter (/= "AA") . Map.keys . Map.filter (== 0) $ vertices g

removeVertex :: Graph -> Vertex -> Graph
removeVertex g v = g {vertices = vertices', edges = edges'}
  where
    vertices' = Map.delete v (vertices g)
    edges' = removeEdges v . updateEdges (edges g) . connectedTunnels g $ v

connectedTunnels :: Graph -> Vertex -> [(Edge, Int)]
connectedTunnels g v = map (\p -> (p, cost p)) paths
  where
    connected = foldl tunnel [] . Map.keys . edges $ g
      where
        tunnel ts t
          | fst t == v = snd t : ts
          | snd t == v = fst t : ts
          | otherwise = ts
    paths = [(a, b) | a <- connected, b <- connected, a < b]
    cost (a, b) = sum . Maybe.mapMaybe (\e -> Map.lookup e (edges g)) $ [edge a v, edge v b]

updateEdges :: Edges -> [(Edge, Int)] -> Edges
updateEdges = foldl (\es (p, c) -> Map.alter (Just . Maybe.maybe c (min c)) p es)

removeEdges :: Vertex -> Edges -> Edges
removeEdges v es = foldl (flip Map.delete) es . filter connected . Map.keys $ es
  where
    connected (a, b) = (a == v) || (b == v)

-- parse input

parseInput :: [Text.Text] -> [Valve]
parseInput = map parseValve

type Parser = MP.Parsec Void Text.Text

parseValve :: Text.Text -> Valve
parseValve input = case MP.runParser grammar "" input of
  Left err -> error (MP.errorBundlePretty err)
  Right sensor -> sensor

grammar :: Parser Valve
grammar =
  Valve
    <$ token "Valve"
    <*> identifier
    <* token "has flow rate="
    <*> number
    <* token ";"
    <* (token "tunnels lead to valves" <|> token "tunnel leads to valve")
    <*> MP.sepBy1 identifier (token ",")

number :: Parser Int
number = L.lexeme C.space L.decimal

token :: Text.Text -> Parser Text.Text
token s = L.lexeme C.space (C.string s)

identifier :: Parser Text.Text
identifier = Text.pack <$> L.lexeme C.space (MP.some C.letterChar)
