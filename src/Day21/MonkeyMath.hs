{-# LANGUAGE OverloadedStrings #-}

module Day21.MonkeyMath where

import qualified AoC.Puzzle as Puzzle
import Control.Applicative (Alternative ((<|>)))
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import Data.Tuple.Extra ((&&&))
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

solver :: Puzzle.Solver
solver = Puzzle.Solver 21 "🙊 Monkey Math" solve

solve :: String -> Puzzle.Solution
solve = (partOne &&& partTwo) . parseInput . Text.lines . Text.pack

-- solution

partOne :: Monkeys -> String
partOne = show . (`calc` "root")

partTwo :: Monkeys -> String
partTwo = show . yell

type Name = Text.Text

data Operand = Plus | Minus | Mult | Div deriving (Eq, Show)

data Job = Number Int | Math Name Operand Name deriving (Eq, Show)

type Monkeys = Map.Map Name Job

calc :: Monkeys -> Name -> Int
calc monkeys name = case Map.lookup name monkeys of
  Nothing -> undefined
  Just job -> evaluate monkeys job

evaluate :: Monkeys -> Job -> Int
evaluate _ (Number n) = n
evaluate monkeys (Math a Plus b) = calc monkeys a + calc monkeys b
evaluate monkeys (Math a Minus b) = calc monkeys a - calc monkeys b
evaluate monkeys (Math a Mult b) = calc monkeys a * calc monkeys b
evaluate monkeys (Math a Div b) = calc monkeys a `div` calc monkeys b

yell :: Monkeys -> Int
yell monkeys = case Map.lookup "root" monkeys of
  Just (Math a _ b) ->
    Maybe.maybe
      (Maybe.maybe undefined (findYell monkeys (calc monkeys a)) (findHuman monkeys b))
      (findYell monkeys (calc monkeys b))
      (findHuman monkeys a)
  _ -> undefined

findYell :: Monkeys -> Int -> [Name] -> Int
findYell _ m ["humn"] = m
findYell monkeys m next = case monkeys Map.! head next of
  (Math a op b) ->
    if a == (next !! 1)
      then findYell monkeys (revertL op (calc monkeys b) m) (tail next)
      else findYell monkeys (revertR op (calc monkeys a) m) (tail next)
  _ -> undefined

revertL :: Operand -> Int -> Int -> Int
revertL Plus b r = r - b
revertL Minus b r = r + b
revertL Mult b r = div r b
revertL Div b r = r * b

revertR :: Operand -> Int -> Int -> Int
revertR Plus a r = r - a
revertR Minus a r = a - r
revertR Mult a r = div r a
revertR Div a r = div a r

findHuman :: Monkeys -> Name -> Maybe [Name]
findHuman _ "humn" = Just ["humn"]
findHuman monkeys name = case Map.lookup name monkeys of
  Nothing -> undefined
  Just (Number _) -> Nothing
  Just (Math a _ b) -> if Maybe.isJust fa then (name :) <$> fa else (name :) <$> fb
    where
      fa = findHuman monkeys a
      fb = findHuman monkeys b

-- parse input

parseInput :: [Text.Text] -> Monkeys
parseInput = Map.fromList . map parseMath

type Parser = MP.Parsec Void Text.Text

parseMath :: Text.Text -> (Name, Job)
parseMath input = case MP.runParser grammar "" input of
  Left err -> error (MP.errorBundlePretty err)
  Right math -> math

grammar :: Parser (Name, Job)
grammar = (,) <$> identifier <* token ":" <*> parseJob

parseJob :: Parser Job
parseJob = (Number <$> number) <|> (Math <$> identifier <*> parseOp <*> identifier)

parseOp :: Parser Operand
parseOp = (Plus <$ token "+") <|> (Minus <$ token "-") <|> (Mult <$ token "*") <|> (Div <$ token "/")

number :: Parser Int
number = L.lexeme C.space L.decimal

token :: Text.Text -> Parser Text.Text
token s = L.lexeme C.space (C.string s)

identifier :: Parser Text.Text
identifier = Text.pack <$> L.lexeme C.space (MP.some C.letterChar)
