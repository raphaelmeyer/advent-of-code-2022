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
partOne (monkeyMap, path) = show (password monkeyMap path)

partTwo :: (MonkeyMap, Path) -> String
partTwo _ = show (0 :: Int)

data Tile = Open | Wall deriving (Eq, Show)

type Grid = Vector.Vector (Vector.Vector Tile)

data Quadrant = Face Grid | Empty deriving (Eq, Show)

data MonkeyMap = MonkeyMap {getMap :: Map.Map Pos Quadrant, getSideLen :: Int} deriving (Eq, Show)

data Instruction = TurnLeft | TurnRight | Forward Int deriving (Eq, Show)

type Path = [Instruction]

type Pos = (Int, Int)

type QPos = (Pos, Pos)

data Facing = FaceRight | FaceDown | FaceLeft | FaceUp deriving (Eq, Show)

password :: MonkeyMap -> Path -> Int
password monkeyMap path = 1000 * row + 4 * col + fValue facing
  where
    (((qx, qy), (x, y)), facing) = follow monkeyMap path
    col = qx * getSideLen monkeyMap + x + 1
    row = qy * getSideLen monkeyMap + y + 1
    fValue FaceRight = 0
    fValue FaceDown = 1
    fValue FaceLeft = 2
    fValue FaceUp = 3

follow :: MonkeyMap -> Path -> (QPos, Facing)
follow monkeyMap = foldl (followStep monkeyMap) (findStart monkeyMap, FaceRight)

followStep :: MonkeyMap -> (QPos, Facing) -> Instruction -> (QPos, Facing)
followStep _ (pos, FaceRight) TurnLeft = (pos, FaceUp)
followStep _ (pos, FaceDown) TurnLeft = (pos, FaceRight)
followStep _ (pos, FaceLeft) TurnLeft = (pos, FaceDown)
followStep _ (pos, FaceUp) TurnLeft = (pos, FaceLeft)
followStep _ (pos, FaceRight) TurnRight = (pos, FaceDown)
followStep _ (pos, FaceDown) TurnRight = (pos, FaceLeft)
followStep _ (pos, FaceLeft) TurnRight = (pos, FaceUp)
followStep _ (pos, FaceUp) TurnRight = (pos, FaceRight)
followStep m (pos, FaceRight) (Forward n) = (moveRight m pos n, FaceRight)
followStep m (pos, FaceDown) (Forward n) = (moveDown m pos n, FaceDown)
followStep m (pos, FaceLeft) (Forward n) = (moveLeft m pos n, FaceLeft)
followStep m (pos, FaceUp) (Forward n) = (moveUp m pos n, FaceUp)

moveRight :: MonkeyMap -> QPos -> Int -> QPos
moveRight _ pos 0 = pos
moveRight m old@((qx, qy), (x, y)) n = case qLookup next m of
  Just Open -> moveRight m next (n - 1)
  Just Wall -> old
  _ -> undefined
  where
    qn = maximum . map fst . Map.keys . getMap $ m
    nextQ = head . dropWhile (\q -> quadrant m q == Empty) . tail $ iterate (\(i, j) -> if i >= qn then (0, j) else (i + 1, j)) (qx, qy)
    next = if x + 1 < getSideLen m then ((qx, qy), (x + 1, y)) else (nextQ, (0, y))

moveDown :: MonkeyMap -> QPos -> Int -> QPos
moveDown _ pos 0 = pos
moveDown m old@((qx, qy), (x, y)) n = case qLookup next m of
  Just Open -> moveDown m next (n - 1)
  Just Wall -> old
  _ -> undefined
  where
    qn = maximum . map snd . Map.keys . getMap $ m
    nextQ = head . dropWhile (\q -> quadrant m q == Empty) . tail $ iterate (\(i, j) -> if j >= qn then (i, 0) else (i, j + 1)) (qx, qy)
    next = if y + 1 < getSideLen m then ((qx, qy), (x, y + 1)) else (nextQ, (x, 0))

moveLeft :: MonkeyMap -> QPos -> Int -> QPos
moveLeft _ pos 0 = pos
moveLeft m old@((qx, qy), (x, y)) n = case qLookup next m of
  Just Open -> moveLeft m next (n - 1)
  Just Wall -> old
  _ -> undefined
  where
    qn = maximum . map fst . Map.keys . getMap $ m
    nextQ = head . dropWhile (\q -> quadrant m q == Empty) . tail $ iterate (\(i, j) -> if i <= 0 then (qn, j) else (i - 1, j)) (qx, qy)
    next = if x - 1 >= 0 then ((qx, qy), (x - 1, y)) else (nextQ, (getSideLen m - 1, y))

moveUp :: MonkeyMap -> QPos -> Int -> QPos
moveUp _ pos 0 = pos
moveUp m old@((qx, qy), (x, y)) n = case qLookup next m of
  Just Open -> moveUp m next (n - 1)
  Just Wall -> old
  _ -> undefined
  where
    qn = maximum . map snd . Map.keys . getMap $ m
    nextQ = head . dropWhile (\q -> quadrant m q == Empty) . tail $ iterate (\(i, j) -> if j <= 0 then (i, qn) else (i, j - 1)) (qx, qy)
    next = if y - 1 >= 0 then ((qx, qy), (x, y - 1)) else (nextQ, (x, getSideLen m - 1))

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
