module Day11.Monkey where

data Monkey = Monkey {items :: [Int], operation :: Operation, test :: Test, inspected :: Int} deriving (Eq, Show)

data Operation = Add Value Value | Mul Value Value deriving (Eq, Show)

data Value = Const Int | Old deriving (Eq, Show)

data Test = Test {div :: Int, whenTrue :: Int, whenFalse :: Int} deriving (Eq, Show)
