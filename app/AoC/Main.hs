{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified AoC.Puzzle as Puzzle
import Control.Applicative ((<**>))
import qualified Control.Applicative as Applicative
import qualified Day01.Calories as Day01
import qualified Day02.RockPaperScissors as Day02
import qualified Day03.Rucksack as Day03
import qualified Day04.Cleanup as Day04
import qualified Day05.SupplyStacks as Day05
import qualified Day06.Tuning as Day06
import qualified Day07.NoSpace as Day07
import qualified Day08.TreeHouse as Day08
import qualified Day09.RopeBridge as Day09
import qualified Day10.CathodeRayTube as Day10
import qualified Day11.KeepAway as Day11
import qualified Day12.HillClimbing as Day12
import qualified Day13.DistressSignal as Day13
import qualified Day14.RegolithReservoir as Day14
import qualified Day15.Beacon as Day15
import qualified Day16.Valves as Day16
import qualified Day17.PyroclasticFlow as Day17
import qualified Day18.BoilingBoulders as Day18
import qualified Day19.Minerals as Day19
import qualified Options.Applicative as Opt

newtype Options = Options {getDay :: Maybe Int} deriving (Show)

options :: Opt.Parser Options
options = Options <$> Applicative.optional (Opt.option Opt.auto $ Opt.long "day" <> Opt.help "run solution of a single day" <> Opt.metavar "DAY")

solutions :: [(Puzzle.Solver, String)]
solutions =
  [ (Day01.solver, "data/day-01.txt"),
    (Day02.solver, "data/day-02.txt"),
    (Day03.solver, "data/day-03.txt"),
    (Day04.solver, "data/day-04.txt"),
    (Day05.solver, "data/day-05.txt"),
    (Day06.solver, "data/day-06.txt"),
    (Day07.solver, "data/day-07.txt"),
    (Day08.solver, "data/day-08.txt"),
    (Day09.solver, "data/day-09.txt"),
    (Day10.solver, "data/day-10.txt"),
    (Day11.solver, "data/day-11.txt"),
    (Day12.solver, "data/day-12.txt"),
    (Day13.solver, "data/day-13.txt"),
    (Day14.solver, "data/day-14.txt"),
    (Day15.solver, "data/day-15.txt"),
    (Day16.solver, "data/day-16.txt"),
    (Day17.solver, "data/day-17.txt"),
    (Day18.solver, "data/day-18.txt"),
    (Day19.solver, "data/day-19.txt")
  ]

year :: String
year = "2022"

title :: String
title = "🎅🎄 Advent of Code " ++ year ++ " 🎄🦌"

main :: IO ()
main = do
  opts <-
    Opt.execParser
      ( Opt.info
          (options <**> Opt.helper)
          (Opt.fullDesc <> Opt.progDesc ("run advent of code " ++ year ++ " solutions") <> Opt.header title)
      )

  putStrLn title
  runSolutions (getDay opts)

runSolutions :: Maybe Int -> IO ()
runSolutions day = mapM_ runSolution . puzzles $ solutions
  where
    puzzles = case day of
      Nothing -> id
      Just day' -> filter ((== day') . Puzzle.day . fst)

runSolution :: (Puzzle.Solver, String) -> IO ()
runSolution (solver, file) = do
  putStrLn ""
  putStrLn $ "✨ Day " ++ show (Puzzle.day solver) ++ " : " ++ Puzzle.name solver ++ " ✨"
  putStrLn ""

  input <- readFile file
  let solution = Puzzle.solve solver input

  putStrLn $ "  ⭐ Part One : " ++ fst solution
  putStrLn $ "  ⭐ Part Two : " ++ snd solution
