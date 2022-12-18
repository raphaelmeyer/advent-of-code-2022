module Day18.BoilingBoulders where

import qualified AoC.Puzzle as Puzzle
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Data.Tuple.Extra ((&&&))
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as C (char, space)
import qualified Text.Megaparsec.Char.Lexer as L

solver :: Puzzle.Solver
solver = Puzzle.Solver 18 "💧 Boiling Boulders" solve

solve :: String -> Puzzle.Solution
solve = (partOne &&& partTwo) . parseInput . Text.lines . Text.pack

data Block = Block {x :: Int, y :: Int, z :: Int} deriving (Eq, Show)

-- solution

partOne :: [Block] -> String
partOne = show . surface

partTwo :: [Block] -> String
partTwo = show . outerSurface

surface :: [Block] -> Int
surface blocks = foldl (\s b -> (+ s) . countFree blocks $ b) 0 blocks

outerSurface :: [Block] -> Int
outerSurface droplet = subtract boxSurface . surface . floodDroplet box $ droplet
  where
    box =
      BoundingBox
        (Block (subtract 1 . minimum . map x $ droplet) (subtract 1 . minimum . map y $ droplet) (subtract 1 . minimum . map z $ droplet))
        (Block ((+ 1) . maximum . map x $ droplet) ((+ 1) . maximum . map y $ droplet) ((+ 1) . maximum . map z $ droplet))
    boxSurface = 2 * (dx * dy + dx * dz + dy * dz)
    dx = (x . maxCorner $ box) - (x . minCorner $ box) + 1
    dy = (y . maxCorner $ box) - (y . minCorner $ box) + 1
    dz = (z . maxCorner $ box) - (z . minCorner $ box) + 1

countFree :: [Block] -> Block -> Int
countFree blocks block = foldl (\c neigh -> if neigh block `elem` blocks then c else c + 1) 0 [top, bottom, left, right, back, front]

data BoundingBox = BoundingBox {minCorner :: Block, maxCorner :: Block} deriving (Eq, Show)

data FloodState = FloodState {lava :: [Block], water :: [Block], queue :: Seq.Seq Block, corners :: BoundingBox} deriving (Eq, Show)

floodDroplet :: BoundingBox -> [Block] -> [Block]
floodDroplet box droplet = water . flood $ FloodState droplet [] (Seq.singleton (minCorner box)) box

flood :: FloodState -> FloodState
flood s = Maybe.maybe s (flood . extendWater (s {queue = Seq.drop 1 (queue s)})) (Seq.lookup 0 (queue s))

extendWater :: FloodState -> Block -> FloodState
extendWater s w = if w `notElem` water s then s {water = w : water s, queue = queue'} else s
  where
    adjacent = filter (inRange (corners s)) . map (\f -> f w) $ [top, bottom, left, right, back, front]
    freeBlocks = filter (`notElem` lava s) . filter (`notElem` water s) $ adjacent
    queue' = foldl (Seq.|>) (queue s) freeBlocks

inRange :: BoundingBox -> Block -> Bool
inRange (BoundingBox (Block minX minY minZ) (Block maxX maxY maxZ)) (Block posX posY posZ) =
  minX <= posX && posX <= maxX && minY <= posY && posY <= maxY && minZ <= posZ && posZ <= maxZ

top :: Block -> Block
top b = b {y = y b - 1}

bottom :: Block -> Block
bottom b = b {y = y b + 1}

left :: Block -> Block
left b = b {x = x b - 1}

right :: Block -> Block
right b = b {x = x b + 1}

back :: Block -> Block
back b = b {z = z b - 1}

front :: Block -> Block
front b = b {z = z b + 1}

-- parse input

parseInput :: [Text.Text] -> [Block]
parseInput = map parseBlock

type Parser = MP.Parsec Void Text.Text

parseBlock :: Text.Text -> Block
parseBlock input = case MP.runParser grammar "" input of
  Left err -> error (MP.errorBundlePretty err)
  Right block -> block

grammar :: Parser Block
grammar = Block <$> number <* C.char ',' <*> number <* C.char ',' <*> number

number :: Parser Int
number = L.lexeme C.space L.decimal
