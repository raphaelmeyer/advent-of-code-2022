module Day03.Rucksack where

import qualified Data.Char as Char
import Data.Functor ((<&>))
import Data.Maybe (mapMaybe)
import qualified Data.Text as Text

type Rucksack = (Text.Text, Text.Text)

run :: IO ()
run = do
  input <- readInput "data/day-03.txt"
  let rucksacks = parseInput input
  let priorities = prioritySum rucksacks

  putStrLn ""
  putStrLn "# Day 03 #"
  putStrLn ""
  putStrLn $ "Part I : " ++ show priorities
  putStrLn $ "Part II : " ++ show (0 :: Int)

readInput :: String -> IO [Text.Text]
readInput filename = readFile filename <&> Text.lines . Text.pack

parseInput :: [Text.Text] -> [Rucksack]
parseInput = map split
  where
    split items = Text.splitAt (middle items) items
    middle = (`div` 2) . Text.length

duplicateItems :: [Rucksack] -> [Char]
duplicateItems = mapMaybe duplicateItem

duplicateItem :: Rucksack -> Maybe Char
duplicateItem (one, two) = Text.find (`Text.elem` two) one

prioritySum :: [Rucksack] -> Int
prioritySum = sum . map priority . duplicateItems

priority :: Char -> Int
priority c
  | Char.isUpper c = Char.ord c - Char.ord 'A' + 27
  | Char.isLower c = Char.ord c - Char.ord 'a' + 1
  | otherwise = 0
