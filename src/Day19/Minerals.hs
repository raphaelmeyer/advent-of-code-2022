{-# LANGUAGE OverloadedStrings #-}

module Day19.Minerals where

import qualified AoC.Puzzle as Puzzle
import Control.Applicative (Alternative ((<|>)))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Tuple.Extra ((&&&))
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as C (space, string)
import qualified Text.Megaparsec.Char.Lexer as L

solver :: Puzzle.Solver
solver = Puzzle.Solver 19 "💎 Not Enough Minerals" solve

solve :: String -> Puzzle.Solution
solve = (partOne &&& partTwo) . parseInput . Text.lines . Text.pack

-- solution

partOne :: [BluePrint] -> String
partOne = show . qualityLevelSum

partTwo :: [BluePrint] -> String
partTwo = show . maxGeodeProduct

data Mineral = Ore | Clay | Obsidian | Geode deriving (Eq, Ord, Show)

data Cost = Cost Mineral Int deriving (Eq, Show)

type Costs = Map.Map Mineral [Cost]

data BluePrint = BluePrint {blueprint :: Int, costs :: Costs} deriving (Eq, Show)

type Minerals = Map.Map Mineral Int

type Robots = Map.Map Mineral Int

data Collect = Collect
  { getBluePrint :: BluePrint,
    getMinerals :: Minerals,
    getRobots :: Robots,
    getTime :: Int,
    maxRobots :: Map.Map Mineral Int,
    skipped :: [Mineral]
  }
  deriving (Eq, Show)

data State = State Minerals Robots Int deriving (Eq, Ord, Show)

type Memo = Map.Map State Int

qualityLevelSum :: [BluePrint] -> Int
qualityLevelSum = sum . map (\bp -> blueprint bp * investigateBlueprint bp 24)

maxGeodeProduct :: [BluePrint] -> Int
maxGeodeProduct = product . map (`investigateBlueprint` 32) . take 3

investigateBlueprint :: BluePrint -> Int -> Int
investigateBlueprint bp time = snd . collectGeode ((\s -> s {getTime = time}) . initialState $ bp) $ Map.empty

collectGeode :: Collect -> Memo -> (Memo, Int)
collectGeode s memo = case Map.lookup ms memo of
  Just g -> (memo, g)
  Nothing ->
    if getTime s == 0
      then (memo, Map.findWithDefault 0 Geode . getMinerals $ s)
      else (\(m, g) -> (Map.insert ms g m, g)) . (\(m, gs) -> (m, maximum gs)) . collectStep s $ memo
  where
    ms = State (getMinerals s) (getRobots s) (getTime s)

collectStep :: Collect -> Memo -> (Memo, [Int])
collectStep s memo = (memo'', noNewRobot : buildNewRobot)
  where
    (memo', buildNewRobot) = if getTime s > 1 then foldl (tryBuild s) (memo, []) (chooseRobots buildable) else (memo, [])
    (memo'', noNewRobot) = (`collectGeode` memo') . updateTime . collectMinerals $ s {skipped = buildable}
    buildable = filter (`canBuildRobot` s) [Geode, Obsidian, Clay, Ore]
    chooseRobots = filter useful . filter (`notElem` skipped s)
    useful robot =
      Map.findWithDefault 0 robot (getRobots s)
        * getTime s
        + Map.findWithDefault 0 robot (getMinerals s)
        < (getTime s * Map.findWithDefault 50 robot (maxRobots s))

initialState :: BluePrint -> Collect
initialState bp = Collect bp Map.empty (Map.singleton Ore 1) 24 (robotLimits bp) []

tryBuild :: Collect -> (Memo, [Int]) -> Mineral -> (Memo, [Int])
tryBuild s (memo, gs) robot = (\(m, g) -> (m, g : gs)) . buildAndCollect memo robot $ s
  where
    buildAndCollect m r = (`collectGeode` m) . launchRobot r . updateTime . collectMinerals . buildRobot r

updateTime :: Collect -> Collect
updateTime s = s {getTime = getTime s - 1}

collectMinerals :: Collect -> Collect
collectMinerals s = s {getMinerals = collected}
  where
    collected = Map.foldlWithKey (\resources robot num -> Map.alter (collect num) robot resources) (getMinerals s) (getRobots s)
    collect num (Just n) = Just (n + num)
    collect num Nothing = Just num

canBuildRobot :: Mineral -> Collect -> Bool
canBuildRobot robot s = all enoughMinerals (askPrice robot s)
  where
    enoughMinerals (Cost resource amount) = Map.findWithDefault 0 resource (getMinerals s) >= amount

askPrice :: Mineral -> Collect -> [Cost]
askPrice robot = (Map.! robot) . costs . getBluePrint

buildRobot :: Mineral -> Collect -> Collect
buildRobot robot s = s {getMinerals = payed, skipped = []}
  where
    payed = foldl pay (getMinerals s) cost
    cost = askPrice robot s
    pay resources (Cost resource amount) = Map.adjust (subtract amount) resource resources

launchRobot :: Mineral -> Collect -> Collect
launchRobot robot s = s {getRobots = Map.alter launch robot (getRobots s)}
  where
    launch (Just n) = Just (n + 1)
    launch Nothing = Just 1

robotLimits :: BluePrint -> Map.Map Mineral Int
robotLimits bp = foldl (\ls r -> Map.insert r (limit r) ls) Map.empty [Obsidian, Clay, Ore]
  where
    limit r = maximum . Map.foldl (foldl (\ls (Cost r' c) -> if r' == r then c : ls else ls)) [] $ costs bp

-- parse input

parseInput :: [Text.Text] -> [BluePrint]
parseInput = map parseBluePrint

type Parser = MP.Parsec Void Text.Text

parseBluePrint :: Text.Text -> BluePrint
parseBluePrint input = case MP.runParser grammar "" input of
  Left err -> error (MP.errorBundlePretty err)
  Right bp -> bp

grammar :: Parser BluePrint
grammar = BluePrint <$> parseId <*> parseRobots

parseId :: Parser Int
parseId = token "Blueprint" *> number <* token ":"

parseRobots :: Parser Costs
parseRobots = Map.fromList <$> MP.some parseRobot

parseRobot :: Parser (Mineral, [Cost])
parseRobot =
  ((,) Ore <$ token "Each ore robot costs" <*> parseCosts <* token ".")
    <|> ((,) Clay <$ token "Each clay robot costs" <*> parseCosts <* token ".")
    <|> ((,) Obsidian <$ token "Each obsidian robot costs" <*> parseCosts <* token ".")
    <|> ((,) Geode <$ token "Each geode robot costs" <*> parseCosts <* token ".")

parseCosts :: Parser [Cost]
parseCosts = MP.sepBy parseCost (token "and")

parseCost :: Parser Cost
parseCost = flip Cost <$> number <*> parseMineral

parseMineral :: Parser Mineral
parseMineral = (Ore <$ token "ore") <|> (Clay <$ token "clay") <|> (Obsidian <$ token "obsidian")

token :: Text.Text -> Parser Text.Text
token s = L.lexeme C.space (C.string s) :: Parser Text.Text

number :: Parser Int
number = L.lexeme C.space L.decimal
