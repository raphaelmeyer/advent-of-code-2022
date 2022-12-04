module Day01.Calories where

import qualified Data.Either as Either (rights)
import Data.Functor ((<&>))
import qualified Data.List as List (sort)
import qualified Data.List.Split as Split
import qualified Data.Text as Text
import qualified Data.Text.Read as Read (decimal)

run :: IO ()
run = do
  input <- readInput "data/day-01.txt"
  let foods = parseInput input

  let most = mostCalories foods
  let topThree = topThreeSum foods

  putStrLn ""
  putStrLn "# Day 01 #"
  putStrLn ""
  putStrLn $ "Part I : " ++ show most
  putStrLn $ "Part II : " ++ show topThree

readInput :: String -> IO [Text.Text]
readInput filename = readFile filename <&> Text.lines . Text.pack

parseInput :: [Text.Text] -> [[Int]]
parseInput = map parseCalories . Split.splitWhen Text.null

parseCalories :: [Text.Text] -> [Int]
parseCalories = map fst . Either.rights . map Read.decimal

mostCalories :: [[Int]] -> Int
mostCalories = maximum . map sum

topThreeSum :: [[Int]] -> Int
topThreeSum = sum . take 3 . reverse . List.sort . map sum
