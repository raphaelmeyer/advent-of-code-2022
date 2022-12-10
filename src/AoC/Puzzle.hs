module AoC.Puzzle where

type Solution = (String, String)

data Solver = Solver
  { day :: Int,
    name :: String,
    solve :: String -> Solution
  }
