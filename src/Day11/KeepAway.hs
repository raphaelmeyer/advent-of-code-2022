{-# LANGUAGE OverloadedStrings #-}

module Day11.KeepAway where

import qualified AoC.Puzzle as Puzzle
import Control.Applicative (Alternative ((<|>)))
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Tuple.Extra ((&&&))
import Data.Void (Void)
import qualified Day11.Monkey as M
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as C (space, string)
import qualified Text.Megaparsec.Char.Lexer as L

solver :: Puzzle.Solver
solver = Puzzle.Solver 11 "🐒 Monkey in the Middle" solve

solve :: String -> Puzzle.Solution
solve = (partOne &&& partTwo) . parseInput . Text.lines . Text.pack

-- solution

type Monkeys = Map.Map Int M.Monkey

data Relief = SlightRelief | NoRelief deriving (Eq, Show)

partOne :: Monkeys -> String
partOne = show . business . playRounds SlightRelief 20

partTwo :: Monkeys -> String
partTwo = show . business . playRounds NoRelief 10000

business :: Monkeys -> Int
business = product . take 2 . reverse . List.sort . Map.foldl (\is m -> M.inspected m : is) []

playRounds :: Relief -> Int -> Monkeys -> Monkeys
playRounds relief rounds monkeys = iterate (monkeyRound strategy) monkeys !! rounds
  where
    strategy = case relief of
      SlightRelief -> slightRelief monkeys
      NoRelief -> noRelief monkeys

monkeyRound :: ReliefStrategy -> Monkeys -> Monkeys
monkeyRound relief monkeys = foldl (monkeyTurn relief) monkeys (Map.keys monkeys)

monkeyTurn :: ReliefStrategy -> Monkeys -> Int -> Monkeys
monkeyTurn relief monkeys key = foldl (inspectItem relief monkey) inspecting items
  where
    items = M.items . (Map.!) monkeys $ key
    inspecting = Map.adjust grabItems key monkeys
    grabItems m@M.Monkey {M.inspected = is} = m {M.items = [], M.inspected = is + length items}
    monkey = (Map.!) inspecting key

inspectItem :: ReliefStrategy -> M.Monkey -> Monkeys -> Int -> Monkeys
inspectItem relief monkey monkeys item = throwItem reliefed target monkeys
  where
    level = worry (M.operation monkey) item
    reliefed = relief level
    target = case mod reliefed (M.div . M.test $ monkey) of
      0 -> M.whenTrue . M.test $ monkey
      _ -> M.whenFalse . M.test $ monkey

throwItem :: Int -> Int -> Monkeys -> Monkeys
throwItem item = Map.adjust (catchItem item)

catchItem :: Int -> M.Monkey -> M.Monkey
catchItem item m@M.Monkey {M.items = items} = m {M.items = items ++ [item]}

worry :: M.Operation -> Int -> Int
worry (M.Add a b) level = opValue a level + opValue b level
worry (M.Mul a b) level = opValue a level * opValue b level

opValue :: M.Value -> Int -> Int
opValue (M.Const v) _ = v
opValue M.Old old = old

type ReliefStrategy = Int -> Int

slightRelief :: Monkeys -> ReliefStrategy
slightRelief _ = (`div` 3)

noRelief :: Monkeys -> ReliefStrategy
noRelief monkeys = (`mod` m)
  where
    m = product . map (M.div . M.test) . Map.elems $ monkeys

-- parse input

parseInput :: [Text.Text] -> Monkeys
parseInput = foldl parseNotes Map.empty . map Text.unwords . Split.splitWhen Text.null

type Parser = MP.Parsec Void Text.Text

parseNotes :: Monkeys -> Text.Text -> Monkeys
parseNotes monkeys input = case MP.runParser grammar "" input of
  Left err -> error (MP.errorBundlePretty err)
  Right (n, monkey) -> Map.insert n monkey monkeys

grammar :: Parser (Int, M.Monkey)
grammar = (,) <$ token "Monkey" <*> number <* token ":" <*> parseMonkey

parseMonkey :: Parser M.Monkey
parseMonkey = M.Monkey <$> parseItems <*> parseOperation <*> parseTest <*> pure 0

parseItems :: Parser [Int]
parseItems = token "Starting items:" *> MP.sepBy number (token ",")

parseOperation :: Parser M.Operation
parseOperation = token "Operation: new =" *> (MP.try add <|> mul)
  where
    add = M.Add <$> value <* token "+" <*> value
    mul = M.Mul <$> value <* token "*" <*> value
    value = (M.Old <$ token "old") <|> (M.Const <$> number)

parseTest :: Parser M.Test
parseTest = M.Test <$ token "Test: divisible by" <*> number <*> ifTrue <*> ifFalse
  where
    ifTrue = token "If true: throw to monkey" *> number
    ifFalse = token "If false: throw to monkey" *> number

token :: Text.Text -> Parser Text.Text
token s = L.lexeme C.space (C.string s) :: Parser Text.Text

number :: Parser Int
number = L.lexeme C.space L.decimal
