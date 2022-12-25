module Day25.Snafu where

import qualified AoC.Puzzle as Puzzle
import qualified Data.List as List
import qualified Data.Text as Text

solver :: Puzzle.Solver
solver = Puzzle.Solver 25 "🧮 Full of Hot Air" solve

solve :: String -> Puzzle.Solution
solve input = (partOne . parseInput . Text.lines . Text.pack $ input, "⭐")

-- solution

partOne :: [Snafu] -> String
partOne = Text.unpack . snprint . snum

data Decimal = DoubleMinus | Minus | Zero | One | Two deriving (Eq, Ord, Show)

type Snafu = [Decimal]

snum :: [Snafu] -> Snafu
snum = foldl snadd zero

snadd :: Snafu -> Snafu -> Snafu
snadd = snaddWithCarry Zero

zero :: Snafu
zero = []

snaddWithCarry :: Decimal -> Snafu -> Snafu -> Snafu
snaddWithCarry Zero [] [] = []
snaddWithCarry carry [] [] = [carry]
snaddWithCarry Zero a [] = a
snaddWithCarry Zero [] b = b
snaddWithCarry carry (a : as) [] = c : snaddWithCarry carry' as []
  where
    (c, carry') = addDecimals [carry, a]
snaddWithCarry carry [] (b : bs) = c : snaddWithCarry carry' [] bs
  where
    (c, carry') = addDecimals [carry, b]
snaddWithCarry carry (a : as) (b : bs) = c : snaddWithCarry carry' as bs
  where
    (c, carry') = addDecimals [carry, a, b]

addDecimals :: [Decimal] -> (Decimal, Decimal)
addDecimals = addDecimals' . List.sort

addDecimals' :: [Decimal] -> (Decimal, Decimal)
addDecimals' ds = case sum . map value $ ds of
  -5 -> (Zero, Minus)
  -4 -> (One, Minus)
  -3 -> (Two, Minus)
  -2 -> (DoubleMinus, Zero)
  -1 -> (Minus, Zero)
  0 -> (Zero, Zero)
  1 -> (One, Zero)
  2 -> (Two, Zero)
  3 -> (DoubleMinus, One)
  4 -> (Minus, One)
  5 -> (Zero, One)
  _ -> undefined

value :: Decimal -> Int
value DoubleMinus = -2
value Minus = -1
value Zero = 0
value One = 1
value Two = 2

snprint :: Snafu -> Text.Text
snprint = foldl (\n d -> Text.cons (decimal d) n) Text.empty

decimal :: Decimal -> Char
decimal DoubleMinus = '='
decimal Minus = '-'
decimal Zero = '0'
decimal One = '1'
decimal Two = '2'

-- parse input

parseInput :: [Text.Text] -> [Snafu]
parseInput = map (Text.foldl toSnafu [])
  where
    toSnafu snafu c = case c of
      '=' -> DoubleMinus : snafu
      '-' -> Minus : snafu
      '0' -> Zero : snafu
      '1' -> One : snafu
      '2' -> Two : snafu
      _ -> snafu
