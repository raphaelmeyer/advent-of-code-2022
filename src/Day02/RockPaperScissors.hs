module Day02.RockPaperScissors where

import qualified AoC.Puzzle as Puzzle
import Control.Applicative (Alternative ((<|>)))
import qualified Data.Text as Text
import Data.Tuple.Extra ((&&&))
import qualified Data.Tuple.Extra as Tuple
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char as C (char, space)

solver :: Puzzle.Solver
solver = Puzzle.Solver 2 "🪨 Rock 📄 Paper ✂️ Scissors" solve

solve :: String -> Puzzle.Solution
solve = Tuple.both (show . totalScore) . (guessShapes &&& chooseShapes) . parseInput . Text.lines . Text.pack

-- solution

data Result = Lose | Draw | Win deriving (Eq, Show)

data Shape = Rock | Paper | Scissors deriving (Eq, Show)

data Round = Round Shape Shape deriving (Eq, Show)

data Strategy = Strategy Shape Result deriving (Eq, Show)

guessShapes :: [Strategy] -> [Round]
guessShapes = map guessShape

chooseShapes :: [Strategy] -> [Round]
chooseShapes = map chooseShape

chooseShape :: Strategy -> Round
chooseShape (Strategy shape Draw) = Round shape shape
chooseShape (Strategy Rock Win) = Round Rock Paper
chooseShape (Strategy Paper Win) = Round Paper Scissors
chooseShape (Strategy Scissors Win) = Round Scissors Rock
chooseShape (Strategy Rock Lose) = Round Rock Scissors
chooseShape (Strategy Paper Lose) = Round Paper Rock
chooseShape (Strategy Scissors Lose) = Round Scissors Paper

totalScore :: [Round] -> Int
totalScore = sum . map roundScore

roundScore :: Round -> Int
roundScore r@(Round _ me) = shapeScore me + outcomeScore r

shapeScore :: Shape -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

outcomeScore :: Round -> Int
outcomeScore (Round Rock Paper) = 6
outcomeScore (Round Paper Scissors) = 6
outcomeScore (Round Scissors Rock) = 6
outcomeScore (Round other me) | other == me = 3
outcomeScore _ = 0

guessShape :: Strategy -> Round
guessShape (Strategy other Lose) = Round other Rock
guessShape (Strategy other Draw) = Round other Paper
guessShape (Strategy other Win) = Round other Scissors

-- parse Input

parseInput :: [Text.Text] -> [Strategy]
parseInput = map parseRound

type Parser = MP.Parsec Void Text.Text

parseRound :: Text.Text -> Strategy
parseRound input = case MP.runParser grammar "" input of
  Left _ -> undefined
  Right r -> r
  where
    pShape :: Parser Shape
    pShape =
      (Rock <$ C.char 'A')
        <|> (Paper <$ C.char 'B')
        <|> (Scissors <$ C.char 'C')
    pResult :: Parser Result
    pResult =
      (Lose <$ C.char 'X')
        <|> (Draw <$ C.char 'Y')
        <|> (Win <$ C.char 'Z')
    grammar :: Parser Strategy
    grammar = Strategy <$> pShape <* C.space <*> pResult
