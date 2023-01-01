{-# LANGUAGE OverloadedStrings #-}

module Day16.Valves where

import qualified AoC.Puzzle as Puzzle
import Control.Applicative (Alternative ((<|>)))
import qualified Control.Monad.State as S
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
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
solve = (partOne &&& partTwo) . makeGraph . parseInput . Text.lines . Text.pack

-- solution

partOne :: Graph -> String
partOne = show . mostPressure

partTwo :: Graph -> String
partTwo = show . withElephant

data Valve = Valve {rate :: Int, tunnels :: [Text.Text]} deriving (Eq, Show)

type Valves = Map.Map Text.Text Valve

type Vertex = Text.Text

type Edge = (Text.Text, Text.Text)

type Vertices = Map.Map Vertex Int

type Edges = Map.Map Edge Int

data Graph = Graph {vertices :: Vertices, edges :: Edges} deriving (Eq, Show)

mostPressure :: Graph -> Int
mostPressure g = maximum . map fst $ openValves g 30 "AA" [] 0

withElephant :: Graph -> Int
withElephant g = maximumFlow sorted
  where
    sorted = List.sortBy (\(a, _) (b, _) -> compare b a) explored
    explored = openValves g 26 "AA" [] 0

openValves :: Graph -> Int -> Vertex -> [Vertex] -> Int -> [(Int, [Vertex])]
openValves g time v opened flow = (flow, opened) : followTunnels
  where
    followTunnels = foldl openValve [] (findTunnels (edges g))
    findTunnels = filter ((`notElem` opened) . fst) . Map.foldlWithKey findTunnel []
    findTunnel ts (a, b) d
      | a == v && b /= "AA" = (b, d) : ts
      | b == v && a /= "AA" = (a, d) : ts
      | otherwise = ts
    openValve paths (v', d) = if time' < 0 then paths else paths ++ openValves g time' v' (v' : opened) flow'
      where
        time' = time - 1 - d
        flow' = flow + time' * (vertices g Map.! v')

maximumFlow :: [(Int, [Vertex])] -> Int
maximumFlow [] = undefined
maximumFlow (a : as) = searchMaxFlow (fst a) 0 [a] as

searchMaxFlow :: Int -> Int -> [(Int, [Vertex])] -> [(Int, [Vertex])] -> Int
searchMaxFlow _ _ _ [] = undefined
searchMaxFlow highest flow as (b : bs) = if result > (highest + fst b) then result else searchMaxFlow highest result (b : as) bs
  where
    result = maximum (flow : flows)
    flows = map (\(f, _) -> f + fst b) . filter (null . List.intersect (snd b) . snd) $ as

-- pre-processing

makeGraph :: Valves -> Graph
makeGraph valves = Graph {vertices = vs, edges = es}
  where
    es = Map.fromList . map (\e -> (e, shortestPath valves e)) $ pairs
    vs = Map.filter (> 0) . Map.map rate $ valves
    labels = "AA" : Map.keys vs
    pairs = [(a, b) | a <- labels, b <- labels, a < b]

data ShortestPath = ShortestPath {distances :: Map.Map Vertex Int, queue :: Seq.Seq Vertex, visited :: Set.Set Vertex}

shortestPath :: Valves -> Edge -> Int
shortestPath valves (from, to) = S.evalState search $ ShortestPath (Map.singleton from 0) (Seq.singleton from) Set.empty
  where
    search = do
      v <- dequeue
      distance <- S.gets ((Map.! v) . distances)
      if v == to
        then return distance
        else do
          S.modify (\s -> foldl (visit distance) s (tunnels (valves Map.! v)))
          search

    dequeue :: S.State ShortestPath Vertex
    dequeue =
      S.state
        ( \s -> case Seq.viewl (queue s) of
            Seq.EmptyL -> undefined
            (v Seq.:< vs) -> (v, s {queue = vs})
        )

    visit :: Int -> ShortestPath -> Text.Text -> ShortestPath
    visit distance s nbor =
      if nbor `notElem` visited s
        then s {distances = Map.insert nbor (distance + 1) (distances s), queue = queue s Seq.|> nbor, visited = Set.insert nbor (visited s)}
        else s

-- parse input

parseInput :: [Text.Text] -> Valves
parseInput = Map.fromList . map parseValve

type Parser = MP.Parsec Void Text.Text

parseValve :: Text.Text -> (Text.Text, Valve)
parseValve input = case MP.runParser grammar "" input of
  Left err -> error (MP.errorBundlePretty err)
  Right valve -> valve

grammar :: Parser (Text.Text, Valve)
grammar =
  (,)
    <$> (token "Valve" *> identifier)
    <*> ( Valve
            <$ token "has flow rate="
            <*> number
            <* token ";"
            <* (token "tunnels lead to valves" <|> token "tunnel leads to valve")
            <*> MP.sepBy1 identifier (token ",")
        )

number :: Parser Int
number = L.lexeme C.space L.decimal

token :: Text.Text -> Parser Text.Text
token s = L.lexeme C.space (C.string s)

identifier :: Parser Text.Text
identifier = Text.pack <$> L.lexeme C.space (MP.some C.letterChar)
