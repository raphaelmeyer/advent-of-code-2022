{-# LANGUAGE OverloadedStrings #-}

module Day05.SupplyStacks where

import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe (listToMaybe, mapMaybe)
import qualified Data.Text as Text
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char as C (space, string)
import qualified Text.Megaparsec.Char.Lexer as L

data Move = Move {getCrates :: Int, getFrom :: Int, getTo :: Int} deriving (Show, Eq)

type Stacks = Map.Map Int [Char]

run :: IO ()
run = do
  input <- readInput "data/day-05.txt"
  let (stacks, procedure) = parseInput input

  let result = rearrange9000 stacks procedure
  let message = topCrates result

  let result' = rearrange9001 stacks procedure
  let message' = topCrates result'

  putStrLn ""
  putStrLn "# Day 05 #"
  putStrLn ""
  putStrLn $ "Part I : " ++ show message
  putStrLn $ "Part II : " ++ show message'

readInput :: String -> IO [Text.Text]
readInput filename = readFile filename <&> Text.lines . Text.pack

rearrange9000 :: Stacks -> [Move] -> Stacks
rearrange9000 = foldl rearrange9000'

rearrange9001 :: Stacks -> [Move] -> Stacks
rearrange9001 = foldl rearrange9001'

rearrange9000' :: Stacks -> Move -> Stacks
rearrange9000' = rearrange reverse

rearrange9001' :: Stacks -> Move -> Stacks
rearrange9001' = rearrange id

rearrange :: ([Char] -> [Char]) -> Stacks -> Move -> Stacks
rearrange strategy stacks m = Map.alter drop' (getTo m) lifted
  where
    crates = case Map.lookup (getFrom m) stacks of
      Just cs -> take (getCrates m) cs
      Nothing -> []
    lifted = Map.adjust (drop (getCrates m)) (getFrom m) stacks
    drop' (Just to) = Just $ strategy crates ++ to
    drop' Nothing = Just crates

topCrates :: Stacks -> [Char]
topCrates = Maybe.mapMaybe Maybe.listToMaybe . Map.elems

-- parse input

parseInput :: [Text.Text] -> (Stacks, [Move])
parseInput ls = (parseStacks stacks, parseMoves . tail $ moves)
  where
    (stacks, moves) = break Text.null ls

parseStacks :: [Text.Text] -> Stacks
parseStacks = foldr parseStackLine Map.empty . init

parseStackLine :: Text.Text -> Stacks -> Stacks
parseStackLine line stacks = foldl insert stacks splitted
  where
    splitted =
      zip [1 ..]
        . Text.unpack
        . Text.filter (`notElem` ("[ ]" :: String))
        . Text.replace "    " "!"
        . Text.stripEnd
        $ line
    insert stacks' (n, token)
      | token == '!' = stacks'
      | otherwise = Map.insertWith (++) n [token] stacks'

parseMoves :: [Text.Text] -> [Move]
parseMoves = map parseMove

type Parser = MP.Parsec Void Text.Text

parseMove :: Text.Text -> Move
parseMove input = case MP.runParser grammar "" input of
  Left _ -> undefined
  Right move -> move

grammar :: Parser Move
grammar = Move <$ token "move" <*> number <* token "from" <*> number <* token "to" <*> number
  where
    token s = L.lexeme C.space (C.string s) :: Parser Text.Text
    number = L.lexeme C.space L.decimal
