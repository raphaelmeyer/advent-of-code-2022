{-# LANGUAGE OverloadedStrings #-}

module Day14.RegolithReservoir where

import qualified AoC.Puzzle as Puzzle
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Tuple.Extra ((&&&))
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as C (char, space, string)
import qualified Text.Megaparsec.Char.Lexer as L

solver :: Puzzle.Solver
solver = Puzzle.Solver 14 "🪨 Regolith Reservoir" solve

solve :: String -> Puzzle.Solution
solve = (partOne &&& partTwo) . parseInput . Text.lines . Text.pack

-- solution

partOne :: Cave -> String
partOne = show . sand . floodCave

partTwo :: Cave -> String
partTwo _ = show (2 :: Int)

type Pos = (Int, Int)

data Unit = Rock | Sand deriving (Eq, Show)

type Rocks = Map.Map Pos Unit

data Cave = Cave {rocks :: Rocks, abyss :: Int} deriving (Eq, Show)

floodCave :: Cave -> Cave
floodCave = iterate' dropSand

sand :: Cave -> Int
sand = Map.size . Map.filter (== Sand) . rocks

dropSand :: Cave -> Maybe Cave
dropSand = fallingSand (500, 0)

fallingSand :: Pos -> Cave -> Maybe Cave
fallingSand (x, y) cave
  | y >= abyss cave = Nothing
  | Map.notMember (x, y + 1) (rocks cave) = fallingSand (x, y + 1) cave
  | Map.notMember (x - 1, y + 1) (rocks cave) = fallingSand (x - 1, y + 1) cave
  | Map.notMember (x + 1, y + 1) (rocks cave) = fallingSand (x + 1, y + 1) cave
  | otherwise = Just cave {rocks = Map.insert (x, y) Sand (rocks cave)}

iterate' :: (a -> Maybe a) -> a -> a
iterate' f a = case f a of
  Just b -> iterate' f b
  Nothing -> a

exploreAbyss :: Rocks -> Int
exploreAbyss = maximum . map snd . Map.keys . Map.filter (== Rock)

-- parse input

parseInput :: [Text.Text] -> Cave
parseInput input = Cave rocks' (exploreAbyss rocks')
  where
    rocks' = foldl makeRockPath Map.empty . map parsePath $ input

makeRockPath :: Rocks -> [Pos] -> Rocks
makeRockPath cave (a : b : corners) = makeRockPath (insertLine a b cave) (b : corners)
makeRockPath cave [_] = cave
makeRockPath _ [] = undefined

insertLine :: Pos -> Pos -> Rocks -> Rocks
insertLine (x1, y1) (x2, y2) cave = foldl (\c k -> Map.insert k Rock c) cave rocks'
  where
    rocks'
      | x1 == x2 = zip (repeat x1) (range y1 y2)
      | y1 == y2 = zip (range x1 x2) (repeat y1)
      | otherwise = undefined
    range a b = if a < b then [a .. b] else [b .. a]

type Parser = MP.Parsec Void Text.Text

parsePath :: Text.Text -> [Pos]
parsePath input = case MP.runParser grammar "" input of
  Left err -> error (MP.errorBundlePretty err)
  Right corners -> corners

grammar :: Parser [Pos]
grammar = MP.sepBy parsePos (token "->")

parsePos :: Parser Pos
parsePos = (,) <$> number <* C.char ',' <*> number

number :: Parser Int
number = L.lexeme C.space L.decimal

token :: Text.Text -> Parser Text.Text
token s = L.lexeme C.space (C.string s)
