{-# LANGUAGE OverloadedStrings #-}

module Day10.CathodeRayTube where

import qualified Data.Bool as Bool
import Data.Functor ((<&>))
import qualified Data.Text as Text
import qualified Data.Text.Read as Read (decimal, signed)

-- runner

run :: IO ()
run = do
  input <- readInput "data/day-10.txt"
  let code = parseInput input
  let rx = evaluateX code
  let signalSum = sumSignal rx

  let screen = drawScreen rx

  putStrLn ""
  putStrLn "# Day 10 #"
  putStrLn ""
  putStrLn $ "Part I : " ++ show signalSum
  putStrLn "Part II : "
  mapM_ (putStrLn . Text.unpack) $ screen

-- solution

data Code = Noop | AddX Int deriving (Eq, Show)

data RegX = RegX {getCycle :: Int, getValue :: Int} deriving (Eq, Show)

sumSignal :: [RegX] -> Int
sumSignal rx = sum . map (`signalValue` rx) $ [20, 60, 100, 140, 180, 220]

evaluateX :: [Code] -> [RegX]
evaluateX = snd . foldl execute (1, [RegX 1 1])

execute :: (Int, [RegX]) -> Code -> (Int, [RegX])
execute (c, rx) Noop = (c + 1, rx)
execute (c, rx) (AddX n) = (c', RegX c' v' : rx)
  where
    c' = c + 2
    v' = (+ n) . getValue . head $ rx

signalValue :: Int -> [RegX] -> Int
signalValue n = (* n) . xValue n

xValue :: Int -> [RegX] -> Int
xValue n = getValue . head . filter ((<= n) . getCycle)

drawScreen :: [RegX] -> [Text.Text]
drawScreen rx = Text.chunksOf 40 . Text.pack . map (Bool.bool ' ' '#' . drawPixel rx) $ [0 .. 239]

drawPixel :: [RegX] -> Int -> Bool
drawPixel rx n = case mod n 40 - xValue (n + 1) rx of
  -1 -> True
  0 -> True
  1 -> True
  _ -> False

-- parse input

readInput :: String -> IO [Text.Text]
readInput filename = readFile filename <&> Text.lines . Text.pack

parseInput :: [Text.Text] -> [Code]
parseInput = map parseInstruction

parseInstruction :: Text.Text -> Code
parseInstruction input
  | Text.isPrefixOf "addx" input = AddX . parseInt . Text.drop 5 $ input
  | otherwise = Noop
  where
    parseInt text = case Read.signed Read.decimal text of
      Right (value, _) -> value
      Left _ -> undefined
