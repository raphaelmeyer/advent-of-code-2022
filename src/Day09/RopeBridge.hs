{-# LANGUAGE OverloadedStrings #-}

module Day09.RopeBridge where

import qualified AoC.Puzzle as Puzzle
import Control.Applicative (Alternative ((<|>)))
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Tuple.Extra ((&&&))
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as C (space, string)
import qualified Text.Megaparsec.Char.Lexer as L

solver :: Puzzle.Solver
solver = Puzzle.Solver 9 "🐍 Rope Bridge" solve

solve :: String -> Puzzle.Solution
solve = (show . countVisitedByTail &&& show . countVisitedByLongTail) . parseInput . Text.lines . Text.pack

-- solution

data Direction = R | L | U | D deriving (Eq, Show)

data Move = Move Direction Int deriving (Eq, Show)

type Pos = (Int, Int)

newtype Rope = Rope [Pos] deriving (Eq, Show)

type Visited = Set.Set Pos

countVisitedByTail :: [Move] -> Int
countVisitedByTail = Set.size . fst . foldl simulate (Set.empty, Rope (replicate 2 (0, 0)))

countVisitedByLongTail :: [Move] -> Int
countVisitedByLongTail = Set.size . fst . foldl simulate (Set.empty, Rope (replicate 10 (0, 0)))

simulate :: (Visited, Rope) -> Move -> (Visited, Rope)
simulate (visited, rope) (Move d n)
  | n > 0 = simulate (simulateStep (visited, rope) d) (Move d (n - 1))
  | otherwise = (visited, rope)

simulateStep :: (Visited, Rope) -> Direction -> (Visited, Rope)
simulateStep (visited, Rope (h : rope)) d = (Set.insert t' visited, Rope rope')
  where
    h' = moveHead h d
    rope' = updateTail h' rope
    t' = last rope'
simulateStep (_, Rope []) _ = undefined

updateTail :: Pos -> [Pos] -> [Pos]
updateTail h [] = [h]
updateTail h (t : rs) = h : updateTail t' rs
  where
    t' = moveTail h t

moveHead :: Pos -> Direction -> Pos
moveHead (x, y) R = (x + 1, y)
moveHead (x, y) L = (x - 1, y)
moveHead (x, y) U = (x, y + 1)
moveHead (x, y) D = (x, y - 1)

moveTail :: Pos -> Pos -> Pos
moveTail h (x, y) = case (dx, dy) of
  (-2, -2) -> (x - 1, y - 1)
  (-2, 2) -> (x - 1, y + 1)
  (2, -2) -> (x + 1, y - 1)
  (2, 2) -> (x + 1, y + 1)
  (-2, 0) -> (x - 1, y)
  (2, 0) -> (x + 1, y)
  (0, -2) -> (x, y - 1)
  (0, 2) -> (x, y + 1)
  (-2, -1) -> (x - 1, y - 1)
  (-2, 1) -> (x - 1, y + 1)
  (2, -1) -> (x + 1, y - 1)
  (2, 1) -> (x + 1, y + 1)
  (-1, -2) -> (x - 1, y - 1)
  (1, -2) -> (x + 1, y - 1)
  (-1, 2) -> (x - 1, y + 1)
  (1, 2) -> (x + 1, y + 1)
  _ -> if inRange then (x, y) else error ("not in range " ++ show (dx, dy))
  where
    dx = fst h - x
    dy = snd h - y
    inRange = -1 <= dx && dx <= 1 && -1 <= dy && dy <= 1

-- parse input

parseInput :: [Text.Text] -> [Move]
parseInput = map parseMove

type Parser = MP.Parsec Void Text.Text

parseMove :: Text.Text -> Move
parseMove input = case MP.runParser grammar "" input of
  Left _ -> undefined
  Right move -> move

grammar :: Parser Move
grammar =
  (Move U <$ token "U" <*> number)
    <|> (Move D <$ token "D" <*> number)
    <|> (Move R <$ token "R" <*> number)
    <|> (Move L <$ token "L" <*> number)
  where
    token s = L.lexeme C.space (C.string s) :: Parser Text.Text
    number = L.lexeme C.space L.decimal
