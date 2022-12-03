module Day02.RockPaperScissors where

import Control.Applicative (Alternative ((<|>)))
import Data.Functor ((<&>))
import qualified Data.Text as Text
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char as C (char, space)

data Result = Lose | Draw | Win deriving (Eq, Show)

data Shape = Rock | Paper | Scissors deriving (Eq, Show)

data Round = Round Shape Shape deriving (Eq, Show)

data Strategy = Strategy Shape Result deriving (Eq, Show)

run :: IO ()
run = do
  input <- readInput "data/day-02.txt"
  let guide = parseInput input
  let guessScore = totalScore . guessGuide $ guide
  let guideScore = totalScore . chooseShapes $ guide

  putStrLn ""
  putStrLn "# Day 02 #"
  putStrLn ""
  putStrLn $ "Part I : " ++ show guessScore
  putStrLn $ "Part II : " ++ show guideScore

readInput :: String -> IO [Text.Text]
readInput filename = readFile filename <&> Text.lines . Text.pack

parseInput :: [Text.Text] -> [Strategy]
parseInput = map parseRound

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

guessGuide :: [Strategy] -> [Round]
guessGuide = map guess
  where
    guess (Strategy other Lose) = Round other Rock
    guess (Strategy other Draw) = Round other Paper
    guess (Strategy other Win) = Round other Scissors

-- parse Input

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
