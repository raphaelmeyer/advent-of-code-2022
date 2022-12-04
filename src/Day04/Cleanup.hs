module Day04.Cleanup where

import Data.Functor ((<&>))
import qualified Data.Text as Text
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char as C (char)
import qualified Text.Megaparsec.Char.Lexer as L

run :: IO ()
run = do
  input <- readInput "data/day-04.txt"
  let pairs = parseInput input
  let fullyContaining = countFullyContaining pairs
  let overlapping = countOverlapping pairs

  putStrLn ""
  putStrLn "# Day 04 #"
  putStrLn ""
  putStrLn $ "Part I : " ++ show fullyContaining
  putStrLn $ "Part II : " ++ show overlapping

type Assignment = (Int, Int)

data Pair = Pair Assignment Assignment deriving (Eq, Show)

readInput :: String -> IO [Text.Text]
readInput filename = readFile filename <&> Text.lines . Text.pack

parseInput :: [Text.Text] -> [Pair]
parseInput = map parsePair

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
