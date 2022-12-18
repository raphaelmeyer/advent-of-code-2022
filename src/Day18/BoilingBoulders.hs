module Day18.BoilingBoulders where

import qualified AoC.Puzzle as Puzzle
import qualified Data.Text as Text
import Data.Tuple.Extra ((&&&))
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as C (char, space)
import qualified Text.Megaparsec.Char.Lexer as L

solver :: Puzzle.Solver
solver = Puzzle.Solver 18 "💧 Boiling Boulders" solve

solve :: String -> Puzzle.Solution
solve = (partOne &&& partTwo) . parseInput . Text.lines . Text.pack

data Block = Block {x :: Int, y :: Int, z :: Int} deriving (Eq, Show)

-- solution

partOne :: [Block] -> String
partOne = show . surface

partTwo :: [Block] -> String
partTwo _ = show (0 :: Int)

surface :: [Block] -> Int
surface blocks = foldl (\s b -> (+ s) . countFree blocks $ b) 0 blocks

countFree :: [Block] -> Block -> Int
countFree blocks block = foldl (\c neigh -> if neigh block `elem` blocks then c else c + 1) 0 [top, bottom, left, right, back, front]

top :: Block -> Block
top b = b {y = y b - 1}

bottom :: Block -> Block
bottom b = b {y = y b + 1}

left :: Block -> Block
left b = b {x = x b - 1}

right :: Block -> Block
right b = b {x = x b + 1}

back :: Block -> Block
back b = b {z = z b - 1}

front :: Block -> Block
front b = b {z = z b + 1}

-- parse input

parseInput :: [Text.Text] -> [Block]
parseInput = map parseBlock

type Parser = MP.Parsec Void Text.Text

parseBlock :: Text.Text -> Block
parseBlock input = case MP.runParser grammar "" input of
  Left err -> error (MP.errorBundlePretty err)
  Right block -> block

grammar :: Parser Block
grammar = Block <$> number <* C.char ',' <*> number <* C.char ',' <*> number

number :: Parser Int
number = L.lexeme C.space L.decimal
