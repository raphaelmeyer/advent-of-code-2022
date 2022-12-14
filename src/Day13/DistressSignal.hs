module Day13.DistressSignal where

import qualified AoC.Puzzle as Puzzle
import Control.Applicative (Alternative ((<|>)))
import qualified Data.List as List
import qualified Data.List.Split as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import Data.Tuple.Extra ((&&&))
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as C (char)
import qualified Text.Megaparsec.Char.Lexer as L

solver :: Puzzle.Solver
solver = Puzzle.Solver 13 "📡 Distress Signal" solve

solve :: String -> Puzzle.Solution
solve = (partOne &&& partTwo) . parseInput . Text.lines . Text.pack

-- solution

type Signal = [Packet]

data Packet = Packet {left :: Data, right :: Data} deriving (Eq, Show)

data Data = Value Int | List [Data] deriving (Eq, Show)

partOne :: Signal -> String
partOne = show . orderSum

partTwo :: Signal -> String
partTwo = show . decoderKey

inOrder :: Packet -> Bool
inOrder p = left p < right p

instance Ord Data where
  compare (Value a) (Value b) = compare a b
  compare (List (a : as)) (List (b : bs)) = case compare a b of
    EQ -> compare as bs
    ord -> ord
  compare (List []) (List []) = EQ
  compare (List []) (List (_ : _)) = LT
  compare (List (_ : _)) (List []) = GT
  compare (Value a) (List b) = compare (List [Value a]) (List b)
  compare (List a) (Value b) = compare (List a) (List [Value b])

indices :: Signal -> [Int]
indices = map fst . filter snd . zip [1 ..] . map inOrder

orderSum :: Signal -> Int
orderSum = sum . indices

two :: Data
two = List [List [Value 2]]

six :: Data
six = List [List [Value 6]]

decoderKey :: Signal -> Int
decoderKey signal = index two * index six
  where
    index d = Maybe.fromMaybe undefined (List.elemIndex d sorted) + 1
    sorted = List.sort . foldl (\ds p -> left p : right p : ds) [two, six] $ signal

-- parse input

parseInput :: [Text.Text] -> Signal
parseInput = map makePacket . List.chunksOf 2 . map parsePacket . filter (not . Text.null)
  where
    makePacket [l, r] = Packet l r
    makePacket _ = undefined

type Parser = MP.Parsec Void Text.Text

parsePacket :: Text.Text -> Data
parsePacket input = case MP.runParser grammar "" input of
  Left err -> error (MP.errorBundlePretty err)
  Right signal -> signal

grammar :: Parser Data
grammar = parseData

parseData :: Parser Data
parseData =
  (Value <$> L.decimal)
    <|> (List <$ C.char '[' <*> MP.sepBy parseData (C.char ',') <* C.char ']')
