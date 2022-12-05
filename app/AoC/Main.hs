module Main where

import Control.Applicative ((<**>))
import qualified Control.Applicative as Applicative
import qualified Data.Map as Map
import qualified Day01.Calories as Day01
import qualified Day02.RockPaperScissors as Day02
import qualified Day03.Rucksack as Day03
import qualified Day04.Cleanup as Day04
import qualified Day05.SupplyStacks as Day05
import qualified Options.Applicative as Opt

newtype Options = Options {getDay :: Maybe Int} deriving (Show)

options :: Opt.Parser Options
options = Options <$> Applicative.optional (Opt.option Opt.auto $ Opt.long "day" <> Opt.help "run solution of a single day" <> Opt.metavar "DAY")

solutions :: Map.Map Int (IO ())
solutions =
  Map.fromList
    [ (1, Day01.run),
      (2, Day02.run),
      (3, Day03.run),
      (4, Day04.run),
      (5, Day05.run)
    ]

main :: IO ()
main = do
  opts <- Opt.execParser (Opt.info (options <**> Opt.helper) (Opt.fullDesc <> Opt.progDesc "run advent of code 2022 solutions" <> Opt.header "Advent of Code 2022"))
  runSolutions (getDay opts)

runSolutions :: Maybe Int -> IO ()
runSolutions Nothing = sequence_ . Map.elems $ solutions
runSolutions (Just day) = runSolution $ Map.lookup day solutions
  where
    runSolution (Just solution) = solution
    runSolution Nothing = putStrLn $ "No solution for day " ++ show day
