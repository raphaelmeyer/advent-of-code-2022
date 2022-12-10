module Day04.Cleanup where

import qualified AoC.Puzzle as Puzzle
import qualified Data.Text as Text
import Data.Tuple.Extra ((&&&))
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char as C (char)
import qualified Text.Megaparsec.Char.Lexer as L

solver :: Puzzle.Solver
solver = Puzzle.Solver 4 "🧹 Camp Cleanup" solve

solve :: String -> Puzzle.Solution
solve = (partOne &&& partTwo) . parseInput . Text.lines . Text.pack

-- solution

partOne :: [Pair] -> String
partOne = show . countFullyContaining

partTwo :: [Pair] -> String
partTwo = show . countOverlapping

type Assignment = (Int, Int)

data Pair = Pair Assignment Assignment deriving (Eq, Show)

countFullyContaining :: [Pair] -> Int
countFullyContaining = length . filter fullyContains
  where
    fullyContains (Pair (a1, b1) (a2, b2))
      | a1 <= a2 && b2 <= b1 = True
      | a2 <= a1 && b1 <= b2 = True
      | otherwise = False

countOverlapping :: [Pair] -> Int
countOverlapping = length . filter overlapping
  where
    overlapping (Pair (a1, b1) (a2, b2))
      | a1 <= a2 && a2 <= b1 = True
      | a2 <= a1 && a1 <= b2 = True
      | otherwise = False

-- parse Input

parseInput :: [Text.Text] -> [Pair]
parseInput = map parsePair

type Parser = MP.Parsec Void Text.Text

parsePair :: Text.Text -> Pair
parsePair input = case MP.runParser grammar "" input of
  Left _ -> undefined
  Right r -> r
  where
    grammar :: Parser Pair
    grammar = Pair <$> pAssignment <* C.char ',' <*> pAssignment
    pAssignment :: Parser Assignment
    pAssignment = (,) <$> L.decimal <* C.char '-' <*> L.decimal
