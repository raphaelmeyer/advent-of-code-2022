{-# LANGUAGE OverloadedStrings #-}

module Day22.MonkeyMap where

import qualified AoC.Puzzle as Puzzle
import Control.Applicative (Alternative ((<|>)))
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import Data.Tuple.Extra ((&&&))
import qualified Data.Vector as Vector
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

solver :: Puzzle.Solver
solver = Puzzle.Solver 22 "🗾 Monkey Map" solve

solve :: String -> Puzzle.Solution
solve = (partOne &&& partTwo) . parseInput . Text.lines . Text.pack

-- solution

partOne :: (MonkeyMap, Path) -> String
partOne (monkeyMap, path) = show (password Flat monkeyMap path)

partTwo :: (MonkeyMap, Path) -> String
partTwo (monkeyMap, path) = show (password Cube monkeyMap path)

data MapType = Flat | Cube deriving (Eq, Show)

data Tile = Open | Wall deriving (Eq, Show)

type Grid = Vector.Vector (Vector.Vector Tile)

data Quadrant = Face Grid | Empty deriving (Eq, Show)

data MonkeyMap = MonkeyMap {getMap :: Map.Map Pos Quadrant, getSideLen :: Int} deriving (Eq, Show)

data Instruction = TurnLeft | TurnRight | Forward Int deriving (Eq, Show)

type Path = [Instruction]

type Pos = (Int, Int)

type QPos = (Pos, Pos)

data Facing = FaceRight | FaceDown | FaceLeft | FaceUp deriving (Eq, Show)

password :: MapType -> MonkeyMap -> Path -> Int
password mapType monkeyMap path = 1000 * row + 4 * col + fValue facing
  where
    (((qx, qy), (x, y)), facing) = follow move monkeyMap path
    col = qx * getSideLen monkeyMap + x + 1
    row = qy * getSideLen monkeyMap + y + 1
    fValue FaceRight = 0
    fValue FaceDown = 1
    fValue FaceLeft = 2
    fValue FaceUp = 3
    move = getMovement monkeyMap mapType

follow :: Move -> MonkeyMap -> Path -> (QPos, Facing)
follow move monkeyMap = foldl (followStep move monkeyMap) (findStart monkeyMap, FaceRight)

followStep :: Move -> MonkeyMap -> (QPos, Facing) -> Instruction -> (QPos, Facing)
followStep _ _ (pos, FaceRight) TurnLeft = (pos, FaceUp)
followStep _ _ (pos, FaceDown) TurnLeft = (pos, FaceRight)
followStep _ _ (pos, FaceLeft) TurnLeft = (pos, FaceDown)
followStep _ _ (pos, FaceUp) TurnLeft = (pos, FaceLeft)
followStep _ _ (pos, FaceRight) TurnRight = (pos, FaceDown)
followStep _ _ (pos, FaceDown) TurnRight = (pos, FaceLeft)
followStep _ _ (pos, FaceLeft) TurnRight = (pos, FaceUp)
followStep _ _ (pos, FaceUp) TurnRight = (pos, FaceRight)
followStep move m (pos, facing) (Forward n) = stepForward move m (pos, facing) n

stepForward :: Move -> MonkeyMap -> (QPos, Facing) -> Int -> (QPos, Facing)
stepForward _ _ pos 0 = pos
stepForward move m pos n = case qLookup (fst next) m of
  Just Open -> stepForward move m next (n - 1)
  Just Wall -> pos
  _ -> error (show pos ++ " -> " ++ show next)
  where
    next = go move pos

findStart :: MonkeyMap -> QPos
findStart monkeyMap = firstOpen . filter (\(_, y) -> y == 0) . Map.keys . getMap $ monkeyMap
  where
    firstOpen [] = undefined
    firstOpen (q : qs) = case quadrant monkeyMap q of
      Empty -> firstOpen qs
      Face grid -> (q, (Maybe.fromMaybe undefined (Vector.elemIndex Open (grid Vector.! 0)), 0))

qLookup :: QPos -> MonkeyMap -> Maybe Tile
qLookup (q, p) m = case quadrant m q of
  Empty -> Nothing
  Face grid -> pLookup grid p

pLookup :: Grid -> Pos -> Maybe Tile
pLookup grid (x, y) = (grid Vector.!? y) >>= (Vector.!? x)

at :: Grid -> Pos -> Tile
at grid (x, y) = (grid Vector.! y) Vector.! x

quadrant :: MonkeyMap -> Pos -> Quadrant
quadrant mm pos = Maybe.fromMaybe Empty (Map.lookup pos . getMap $ mm)

-- moving and wrapping

newtype Move = Move {go :: (QPos, Facing) -> (QPos, Facing)}

getMovement :: MonkeyMap -> MapType -> Move
getMovement monkeyMap mapType = getMovement' mapType len folding
  where
    len = getSideLen monkeyMap
    folding = Map.keys . getMap $ monkeyMap

getMovement' :: MapType -> Int -> [Pos] -> Move
getMovement' Flat len [(0, 1), (1, 1), (2, 0), (2, 1), (2, 2), (3, 2)] = Move {go = exampleFlat len}
getMovement' Cube len [(0, 1), (1, 1), (2, 0), (2, 1), (2, 2), (3, 2)] = Move {go = exampleCube len}
getMovement' Flat len [(0, 2), (0, 3), (1, 0), (1, 1), (1, 2), (2, 0)] = Move {go = inputFlat len}
getMovement' Cube len [(0, 2), (0, 3), (1, 0), (1, 1), (1, 2), (2, 0)] = Move {go = inputCube len}
getMovement' _ _ f = error ("movement for folding " ++ show f ++ " is left as an exercise to the reader")

-- move and wrap on example flat map

exampleFlat :: Int -> (QPos, Facing) -> (QPos, Facing)
exampleFlat len ((q, (x, y)), FaceRight)
  | x == len - 1 && q == (0, 1) = (((1, 1), (0, y)), FaceRight)
  | x == len - 1 && q == (1, 1) = (((2, 1), (0, y)), FaceRight)
  | x == len - 1 && q == (2, 0) = (((2, 0), (0, y)), FaceRight)
  | x == len - 1 && q == (2, 1) = (((0, 1), (0, y)), FaceRight)
  | x == len - 1 && q == (2, 2) = (((3, 2), (0, y)), FaceRight)
  | x == len - 1 && q == (3, 2) = (((2, 2), (0, y)), FaceRight)
  | otherwise = ((q, (x + 1, y)), FaceRight)
exampleFlat len ((q, (x, y)), FaceLeft)
  | x == 0 && q == (0, 1) = (((2, 1), (len - 1, y)), FaceLeft)
  | x == 0 && q == (1, 1) = (((0, 1), (len - 1, y)), FaceLeft)
  | x == 0 && q == (2, 0) = (((2, 0), (len - 1, y)), FaceLeft)
  | x == 0 && q == (2, 1) = (((1, 1), (len - 1, y)), FaceLeft)
  | x == 0 && q == (2, 2) = (((3, 2), (len - 1, y)), FaceLeft)
  | x == 0 && q == (3, 2) = (((2, 2), (len - 1, y)), FaceLeft)
  | otherwise = ((q, (x - 1, y)), FaceLeft)
exampleFlat len ((q, (x, y)), FaceDown)
  | y == len - 1 && q == (0, 1) = (((0, 1), (x, 0)), FaceDown)
  | y == len - 1 && q == (1, 1) = (((1, 1), (x, 0)), FaceDown)
  | y == len - 1 && q == (2, 0) = (((2, 1), (x, 0)), FaceDown)
  | y == len - 1 && q == (2, 1) = (((2, 2), (x, 0)), FaceDown)
  | y == len - 1 && q == (2, 2) = (((2, 0), (x, 0)), FaceDown)
  | y == len - 1 && q == (3, 2) = (((3, 2), (x, 0)), FaceDown)
  | otherwise = ((q, (x, y + 1)), FaceDown)
exampleFlat len ((q, (x, y)), FaceUp)
  | y == 0 && q == (0, 1) = (((0, 1), (x, len - 1)), FaceUp)
  | y == 0 && q == (1, 1) = (((1, 1), (x, len - 1)), FaceUp)
  | y == 0 && q == (2, 0) = (((2, 2), (x, len - 1)), FaceUp)
  | y == 0 && q == (2, 1) = (((2, 0), (x, len - 1)), FaceUp)
  | y == 0 && q == (2, 2) = (((2, 1), (x, len - 1)), FaceUp)
  | y == 0 && q == (3, 2) = (((3, 2), (x, len - 1)), FaceUp)
  | otherwise = ((q, (x, y - 1)), FaceUp)

-- move and wrap on example cube

exampleCube :: Int -> (QPos, Facing) -> (QPos, Facing)
exampleCube len ((q, (x, y)), FaceRight)
  | x == len - 1 && q == (0, 1) = (((1, 1), (0, y)), FaceRight)
  | x == len - 1 && q == (1, 1) = (((2, 1), (0, y)), FaceRight)
  | x == len - 1 && q == (2, 0) = (((3, 2), (len - 1, len - 1 - y)), FaceLeft)
  | x == len - 1 && q == (2, 1) = (((3, 2), (len - 1 - y, 0)), FaceDown)
  | x == len - 1 && q == (2, 2) = (((3, 2), (0, y)), FaceRight)
  | x == len - 1 && q == (3, 2) = (((2, 0), (len - 1, len - 1 - y)), FaceLeft)
  | otherwise = ((q, (x + 1, y)), FaceRight)
exampleCube len ((q, (x, y)), FaceLeft)
  | x == 0 && q == (0, 1) = (((3, 2), (len - 1 - y, len - 1)), FaceUp)
  | x == 0 && q == (1, 1) = (((0, 1), (len - 1, y)), FaceLeft)
  | x == 0 && q == (2, 0) = (((1, 1), (y, 0)), FaceDown)
  | x == 0 && q == (2, 1) = (((1, 1), (len - 1, y)), FaceLeft)
  | x == 0 && q == (2, 2) = (((1, 1), (len - 1 - y, len - 1)), FaceUp)
  | x == 0 && q == (3, 2) = (((2, 2), (len - 1, y)), FaceLeft)
  | otherwise = ((q, (x - 1, y)), FaceLeft)
exampleCube len ((q, (x, y)), FaceDown)
  | y == len - 1 && q == (0, 1) = (((2, 2), (len - 1 - x, len - 1)), FaceUp)
  | y == len - 1 && q == (1, 1) = (((2, 2), (0, x)), FaceRight)
  | y == len - 1 && q == (2, 0) = (((2, 1), (x, 0)), FaceDown)
  | y == len - 1 && q == (2, 1) = (((2, 2), (x, 0)), FaceDown)
  | y == len - 1 && q == (2, 2) = (((0, 1), (len - 1 - x, len - 1)), FaceUp)
  | y == len - 1 && q == (3, 2) = (((0, 1), (0, len - 1 - x)), FaceRight)
  | otherwise = ((q, (x, y + 1)), FaceDown)
exampleCube len ((q, (x, y)), FaceUp)
  | y == 0 && q == (0, 1) = (((2, 0), (len - 1 - x, 0)), FaceDown)
  | y == 0 && q == (1, 1) = (((2, 0), (0, x)), FaceRight)
  | y == 0 && q == (2, 0) = (((0, 1), (len - 1 - x, 0)), FaceDown)
  | y == 0 && q == (2, 1) = (((2, 0), (x, len - 1)), FaceUp)
  | y == 0 && q == (2, 2) = (((2, 1), (x, len - 1)), FaceUp)
  | y == 0 && q == (3, 2) = (((2, 1), (len - 1, len - 1 - x)), FaceLeft)
  | otherwise = ((q, (x, y - 1)), FaceUp)

-- move and wrap on input flat map

inputFlat :: Int -> (QPos, Facing) -> (QPos, Facing)
inputFlat len ((q, (x, y)), FaceRight)
  | x == len - 1 && q == (0, 2) = (((1, 2), (0, y)), FaceRight)
  | x == len - 1 && q == (0, 3) = (((0, 3), (0, y)), FaceRight)
  | x == len - 1 && q == (1, 0) = (((2, 0), (0, y)), FaceRight)
  | x == len - 1 && q == (1, 1) = (((1, 1), (0, y)), FaceRight)
  | x == len - 1 && q == (1, 2) = (((0, 2), (0, y)), FaceRight)
  | x == len - 1 && q == (2, 0) = (((1, 0), (0, y)), FaceRight)
  | otherwise = ((q, (x + 1, y)), FaceRight)
inputFlat len ((q, (x, y)), FaceLeft)
  | x == 0 && q == (0, 2) = (((1, 2), (len - 1, y)), FaceLeft)
  | x == 0 && q == (0, 3) = (((0, 3), (len - 1, y)), FaceLeft)
  | x == 0 && q == (1, 0) = (((2, 0), (len - 1, y)), FaceLeft)
  | x == 0 && q == (1, 1) = (((1, 1), (len - 1, y)), FaceLeft)
  | x == 0 && q == (1, 2) = (((0, 2), (len - 1, y)), FaceLeft)
  | x == 0 && q == (2, 0) = (((1, 0), (len - 1, y)), FaceLeft)
  | otherwise = ((q, (x - 1, y)), FaceLeft)
inputFlat len ((q, (x, y)), FaceDown)
  | y == len - 1 && q == (0, 2) = (((0, 3), (x, 0)), FaceDown)
  | y == len - 1 && q == (0, 3) = (((0, 2), (x, 0)), FaceDown)
  | y == len - 1 && q == (1, 0) = (((1, 1), (x, 0)), FaceDown)
  | y == len - 1 && q == (1, 1) = (((1, 2), (x, 0)), FaceDown)
  | y == len - 1 && q == (1, 2) = (((1, 0), (x, 0)), FaceDown)
  | y == len - 1 && q == (2, 0) = (((2, 0), (x, 0)), FaceDown)
  | otherwise = ((q, (x, y + 1)), FaceDown)
inputFlat len ((q, (x, y)), FaceUp)
  | y == 0 && q == (0, 2) = (((0, 3), (x, len - 1)), FaceUp)
  | y == 0 && q == (0, 3) = (((0, 2), (x, len - 1)), FaceUp)
  | y == 0 && q == (1, 0) = (((1, 2), (x, len - 1)), FaceUp)
  | y == 0 && q == (1, 1) = (((1, 0), (x, len - 1)), FaceUp)
  | y == 0 && q == (1, 2) = (((1, 1), (x, len - 1)), FaceUp)
  | y == 0 && q == (2, 0) = (((2, 0), (x, len - 1)), FaceUp)
  | otherwise = ((q, (x, y - 1)), FaceUp)

-- move and wrap on input cube

inputCube :: Int -> (QPos, Facing) -> (QPos, Facing)
inputCube len ((q, (x, y)), FaceRight)
  | x == len - 1 && q == (0, 2) = (((1, 2), (0, y)), FaceRight)
  | x == len - 1 && q == (0, 3) = (((1, 2), (y, len - 1)), FaceUp)
  | x == len - 1 && q == (1, 0) = (((2, 0), (0, y)), FaceRight)
  | x == len - 1 && q == (1, 1) = (((2, 0), (y, len - 1)), FaceUp)
  | x == len - 1 && q == (1, 2) = (((2, 0), (len - 1, len - 1 - y)), FaceLeft)
  | x == len - 1 && q == (2, 0) = (((1, 2), (len - 1, len - 1 - y)), FaceLeft)
  | otherwise = ((q, (x + 1, y)), FaceRight)
inputCube len ((q, (x, y)), FaceLeft)
  | x == 0 && q == (0, 2) = (((1, 0), (0, len - 1 - y)), FaceRight)
  | x == 0 && q == (0, 3) = (((1, 0), (y, 0)), FaceDown)
  | x == 0 && q == (1, 0) = (((0, 2), (0, len - 1 - y)), FaceRight)
  | x == 0 && q == (1, 1) = (((0, 2), (y, 0)), FaceDown)
  | x == 0 && q == (1, 2) = (((0, 2), (len - 1, y)), FaceLeft)
  | x == 0 && q == (2, 0) = (((1, 0), (len - 1, y)), FaceLeft)
  | otherwise = ((q, (x - 1, y)), FaceLeft)
inputCube len ((q, (x, y)), FaceDown)
  | y == len - 1 && q == (0, 2) = (((0, 3), (x, 0)), FaceDown)
  | y == len - 1 && q == (0, 3) = (((2, 0), (x, 0)), FaceDown)
  | y == len - 1 && q == (1, 0) = (((1, 1), (x, 0)), FaceDown)
  | y == len - 1 && q == (1, 1) = (((1, 2), (x, 0)), FaceDown)
  | y == len - 1 && q == (1, 2) = (((0, 3), (len - 1, x)), FaceLeft)
  | y == len - 1 && q == (2, 0) = (((1, 1), (len - 1, x)), FaceLeft)
  | otherwise = ((q, (x, y + 1)), FaceDown)
inputCube len ((q, (x, y)), FaceUp)
  | y == 0 && q == (0, 2) = (((1, 1), (0, x)), FaceRight)
  | y == 0 && q == (0, 3) = (((0, 2), (x, len - 1)), FaceUp)
  | y == 0 && q == (1, 0) = (((0, 3), (0, x)), FaceRight)
  | y == 0 && q == (1, 1) = (((1, 0), (x, len - 1)), FaceUp)
  | y == 0 && q == (1, 2) = (((1, 1), (x, len - 1)), FaceUp)
  | y == 0 && q == (2, 0) = (((0, 3), (x, len - 1)), FaceUp)
  | otherwise = ((q, (x, y - 1)), FaceUp)

-- parse input

parseInput :: [Text.Text] -> (MonkeyMap, Path)
parseInput input = (parseMap dim board, parsePath description)
  where
    (board, description) = splitInput input
    dim = dimensions board

splitInput :: [Text.Text] -> ([Text.Text], Text.Text)
splitInput input = (takeWhile (not . Text.null) input, last input)

dimensions :: [Text.Text] -> (Int, Int)
dimensions input
  | w * 3 == h * 4 = (4, 3)
  | w * 4 == h * 3 = (3, 4)
  | w * 2 == h * 5 = (5, 2)
  | w * 5 == h * 2 = (2, 5)
  | otherwise = undefined
  where
    (w, h) = (maximum . map Text.length $ input, length input)

parseMap :: (Int, Int) -> [Text.Text] -> MonkeyMap
parseMap dim input = MonkeyMap {getMap = parsed, getSideLen = len}
  where
    len = sideLength dim input
    parsed = Map.fromList . filter ((/= Empty) . snd) . foldl parseRow [] . zip [0 ..] . map List.transpose . Split.chunksOf len . map (Text.chunksOf len) $ input

sideLength :: (Int, Int) -> [Text.Text] -> Int
sideLength (w, h) input = if s1 /= s2 then undefined else s1
  where
    (cols, rows) = (maximum . map Text.length $ input, length input)
    s1 = div cols w
    s2 = div rows h

parseRow :: [((Int, Int), Quadrant)] -> (Int, [[Text.Text]]) -> [((Int, Int), Quadrant)]
parseRow qs (row, cols) = foldl (parseCol row) qs . zip [0 ..] $ cols

parseCol :: Int -> [((Int, Int), Quadrant)] -> (Int, [Text.Text]) -> [((Int, Int), Quadrant)]
parseCol row qs (col, face) = ((col, row), parseFace face) : qs

parseFace :: [Text.Text] -> Quadrant
parseFace rows = if Text.all (== ' ') . head $ rows then Empty else Face . Vector.fromList . foldr parseTiles [] $ rows

parseTiles :: Text.Text -> [Vector.Vector Tile] -> [Vector.Vector Tile]
parseTiles row tiles = Vector.fromList (Text.foldr parseTile [] row) : tiles

parseTile :: Char -> [Tile] -> [Tile]
parseTile '.' tiles = Open : tiles
parseTile '#' tiles = Wall : tiles
parseTile _ _ = undefined

type Parser = MP.Parsec Void Text.Text

parsePath :: Text.Text -> Path
parsePath input = case MP.runParser grammar "" input of
  Left err -> error (MP.errorBundlePretty err)
  Right path -> path

grammar :: Parser Path
grammar = MP.some ((TurnLeft <$ token "L") <|> (TurnRight <$ token "R") <|> (Forward <$> number))

number :: Parser Int
number = L.lexeme C.space L.decimal

token :: Text.Text -> Parser Text.Text
token s = L.lexeme C.space (C.string s)
