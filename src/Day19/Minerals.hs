{-# LANGUAGE OverloadedStrings #-}

module Day19.Minerals where

import qualified AoC.Puzzle as Puzzle
import Control.Applicative (Alternative ((<|>)))
import qualified Control.Monad.State as S
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
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

partOne :: [Blueprint] -> String
partOne = show . qualityLevelSum

partTwo :: [Blueprint] -> String
partTwo = show . maxGeodeProduct

data Mineral = Ore | Clay | Obsidian | Geode deriving (Eq, Ord, Show)

data Cost = Cost Mineral Int deriving (Eq, Show)

type BuildCosts = Map.Map Mineral [Cost]

data Blueprint = Blueprint {blueprintId :: Int, buildCosts :: BuildCosts} deriving (Eq, Show)

type Minerals = Map.Map Mineral Int

type Robots = Map.Map Mineral Int

data Configuration = Configuration {getBlueprint :: Blueprint, maxRobots :: Map.Map Mineral Int} deriving (Eq, Show)

data State = State {getMinerals :: Minerals, getRobots :: Robots, getTime :: Int} deriving (Eq, Show)

qualityLevelSum :: [Blueprint] -> Int
qualityLevelSum = sum . map (\blueprint -> blueprintId blueprint * investigateBlueprint blueprint 24)

maxGeodeProduct :: [Blueprint] -> Int
maxGeodeProduct = product . map (`investigateBlueprint` 32) . take 3

investigateBlueprint :: Blueprint -> Int -> Int
investigateBlueprint blueprint time = mineMinerals config (State {getMinerals = Map.empty, getRobots = Map.singleton Ore 1, getTime = time})
  where
    config = configuration blueprint

configuration :: Blueprint -> Configuration
configuration blueprint = Configuration blueprint (robotLimits blueprint)

mineMinerals :: Configuration -> State -> Int
mineMinerals config s
  | getTime s == 0 = Map.findWithDefault 0 Geode . getMinerals $ s
  | null selected = mineMinerals config collectRemaining
  | otherwise = maximum . map (mineMinerals config) . buildRobots $ selected
  where
    buildRobots = map (\robot -> S.execState (mineAndBuild config robot) s)
    collectRemaining = S.execState (collectMinerals . getTime $ s) s
    selected = filter (contributing s) . filter (useful config s) . filter (buildable config s) $ [Geode, Obsidian, Clay, Ore]

mineAndBuild :: Configuration -> Mineral -> S.State State ()
mineAndBuild config robot = do
  s <- S.get
  time <- requiredTime config robot
  if time <= getTime s
    then do
      collectMinerals time
      buildRobot config robot
    else collectMinerals (getTime s)

collectMinerals :: Int -> S.State State ()
collectMinerals time = S.modify (\s -> s {getMinerals = collected s, getTime = getTime s - time})
  where
    collected s = Map.foldlWithKey (\minerals robot count -> Map.alter (collect (count * time)) robot minerals) (getMinerals s) (getRobots s)
    collect amount (Just n) = Just (amount + n)
    collect amount Nothing = Just amount

buildRobot :: Configuration -> Mineral -> S.State State ()
buildRobot config robot = S.modify (\s -> s {getMinerals = payed s, getRobots = built s})
  where
    payed s = foldl (\minerals (Cost mineral amount) -> Map.adjust (subtract amount) mineral minerals) (getMinerals s) costs
    built s = Map.alter (Just . Maybe.maybe 1 (+ 1)) robot (getRobots s)
    costs = (Map.! robot) . buildCosts . getBlueprint $ config

checkShouldBuild :: Configuration -> Mineral -> S.State State Bool
checkShouldBuild config robot = S.gets (\s -> buildable config s robot && useful config s robot && contributing s robot)

buildable :: Configuration -> State -> Mineral -> Bool
buildable config s robot = all (\(Cost mineral _) -> Map.findWithDefault 0 mineral robots > 0) costs
  where
    costs = (Map.! robot) . buildCosts . getBlueprint $ config
    robots = getRobots s

useful :: Configuration -> State -> Mineral -> Bool
useful config s robot =
  Map.findWithDefault 0 robot (getRobots s)
    * getTime s
    + Map.findWithDefault 0 robot (getMinerals s)
    < (getTime s * Map.findWithDefault 50 robot (maxRobots config))

contributing :: State -> Mineral -> Bool
contributing s Geode = getTime s > 1
contributing s _ = getTime s > 2

requiredTime :: Configuration -> Mineral -> S.State State Int
requiredTime config robot = do
  s <- S.get
  case required s of
    [] -> return 1
    rm -> return . maximum . map (requiredSteps s) $ rm
  where
    required s = requiredMinerals ((buildCosts . getBlueprint $ config) Map.! robot) (getMinerals s)
    requiredSteps s (mineral, missing) = case divMod missing (getRobots s Map.! mineral) of
      (steps, 0) -> steps + 1
      (steps, _) -> steps + 2

requiredMinerals :: [Cost] -> Minerals -> [(Mineral, Int)]
requiredMinerals costs minerals = filter ((> 0) . snd) . foldl requiredMineral [] $ costs
  where
    requiredMineral required (Cost mineral amount) = (mineral, max 0 (amount - Map.findWithDefault 0 mineral minerals)) : required

robotLimits :: Blueprint -> Map.Map Mineral Int
robotLimits blueprint = foldl (\limits robot -> Map.insert robot (limit robot) limits) Map.empty [Obsidian, Clay, Ore]
  where
    limit r = maximum . Map.foldl (requires r) [] . buildCosts $ blueprint
    requires mineral = foldl (\required (Cost m c) -> if m == mineral then c : required else required)

-- parse input

parseInput :: [Text.Text] -> [Blueprint]
parseInput = map parseBlueprint

type Parser = MP.Parsec Void Text.Text

parseBlueprint :: Text.Text -> Blueprint
parseBlueprint input = case MP.runParser grammar "" input of
  Left err -> error (MP.errorBundlePretty err)
  Right bp -> bp

grammar :: Parser Blueprint
grammar = Blueprint <$> parseId <*> parseRobots

parseId :: Parser Int
parseId = token "Blueprint" *> number <* token ":"

parseRobots :: Parser BuildCosts
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
