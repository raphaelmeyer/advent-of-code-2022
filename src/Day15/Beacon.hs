{-# LANGUAGE OverloadedStrings #-}

module Day15.Beacon where

import qualified AoC.Puzzle as Puzzle
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import Data.Tuple.Extra ((&&&))
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as C (space, string)
import qualified Text.Megaparsec.Char.Lexer as L

solver :: Puzzle.Solver
solver = Puzzle.Solver 15 "🔆 Beacon Exclusion Zone" solve

solve :: String -> Puzzle.Solution
solve = (partOne &&& partTwo) . parseInput . Text.lines . Text.pack

-- solution

partOne :: [Sensor] -> String
partOne = show . coveredInRow 2000000

partTwo :: [Sensor] -> String
partTwo = show . tuningFrequency (0, 4000000)

type Pos = (Int, Int)

type Range = (Int, Int)

data Sensor = Sensor {pos :: Pos, beacon :: Pos} deriving (Eq, Show)

tuningFrequency :: Range -> [Sensor] -> Int
tuningFrequency range@(start, end) sensors = 4000000 * x + y
  where
    scanned = trim range . map (`scanRow` sensors) $ [start .. end]
    y = Maybe.fromMaybe undefined (List.findIndex gap scanned)
    x = (+ 1) . snd . head $ (scanned !! y)

trim :: Range -> [[Range]] -> [[Range]]
trim (start, end) = map trimRow
  where
    trimRow = foldl trimRange []
    trimRange rs (from, to)
      | to < start = rs
      | from > end = rs
      | otherwise = (max start from, min to end) : rs

gap :: [Range] -> Bool
gap = (== 2) . length

coveredInRow :: Int -> [Sensor] -> Int
coveredInRow row = intervalSize . scanRow row

scanRow :: Int -> [Sensor] -> [Range]
scanRow row = mergeSegments . Maybe.mapMaybe (coveredSegment row)

mergeSegments :: [Range] -> [Range]
mergeSegments segments = foldl merge [] sorted
  where
    sorted = List.sortBy (\a b -> compare (fst a) (fst b)) segments
    merge [] (from, to) = [(from, to)]
    merge intervals@(i : is) (from, to)
      | snd i < from = (from, to) : intervals
      | snd i >= to = intervals
      | otherwise = (fst i, to) : is

coveredSegment :: Int -> Sensor -> Maybe Range
coveredSegment row (Sensor (xs, ys) (xb, yb))
  | r >= 0 = Just (xs - r, xs + r)
  | otherwise = Nothing
  where
    d = abs (xs - xb) + abs (ys - yb)
    r = d - abs (row - ys)

intervalSize :: [Range] -> Int
intervalSize = foldl (\s (from, to) -> s + to - from) 0

-- parse input

parseInput :: [Text.Text] -> [Sensor]
parseInput = map parseSensor

type Parser = MP.Parsec Void Text.Text

parseSensor :: Text.Text -> Sensor
parseSensor input = case MP.runParser grammar "" input of
  Left err -> error (MP.errorBundlePretty err)
  Right sensor -> sensor

grammar :: Parser Sensor
grammar = Sensor <$ token "Sensor at" <*> parsePosition <* token ":" <* token "closest beacon is at" <*> parsePosition

parsePosition :: Parser Pos
parsePosition = (,) <$ token "x=" <*> integer <* token "," <* token "y=" <*> integer

integer :: Parser Int
integer = L.signed C.space (L.lexeme C.space L.decimal)

token :: Text.Text -> Parser Text.Text
token s = L.lexeme C.space (C.string s) :: Parser Text.Text
