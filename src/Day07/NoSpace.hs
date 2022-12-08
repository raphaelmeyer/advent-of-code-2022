{-# LANGUAGE OverloadedStrings #-}

module Day07.NoSpace where

import Control.Applicative (Alternative ((<|>)))
import Data.Functor ((<&>))
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as C (asciiChar, space, string)
import qualified Text.Megaparsec.Char.Lexer as L

-- runner

run :: IO ()
run = do
  input <- readInput "data/day-07.txt"
  let fs = parseInput input
  let smallSum = smallFolderSum fs
  let toDelete = smallestBigEnough fs

  putStrLn ""
  putStrLn "# Day 07 #"
  putStrLn ""
  putStrLn $ "Part I : " ++ show smallSum
  putStrLn $ "Part II : " ++ show toDelete

-- solution

type Path = [Text.Text]

type Size = Int

data File = File Text.Text Path Size deriving (Eq, Show)

data FileSystem = FileSystem {getFolders :: Set.Set Path, getFiles :: [File]} deriving (Eq, Show)

filesystemSize :: Int
filesystemSize = 70000000

updateSize :: Int
updateSize = 30000000

folderSizes :: FileSystem -> [Int]
folderSizes (FileSystem {getFolders = folders, getFiles = files}) = map folderSize (Set.toList folders)
  where
    folderSize path = foldl (countSize path) 0 files
    countSize path total (File _ filepath size)
      | path `List.isSuffixOf` filepath = total + size
      | otherwise = total

smallFolderSum :: FileSystem -> Int
smallFolderSum = sum . filter (<= 100000) . folderSizes

smallestBigEnough :: FileSystem -> Int
smallestBigEnough fs = minimum candidates
  where
    sizes = folderSizes fs
    free = filesystemSize - maximum sizes
    required = updateSize - free
    candidates = filter (>= required) sizes

-- parse input

readInput :: String -> IO [Text.Text]
readInput filename = readFile filename <&> Text.lines . Text.pack

parseInput :: [Text.Text] -> FileSystem
parseInput = buildFileSystem . map parseLine

buildFileSystem :: [Line] -> FileSystem
buildFileSystem = snd . foldl build ([], FileSystem {getFolders = Set.singleton [], getFiles = []})

build :: (Path, FileSystem) -> Line -> (Path, FileSystem)
build (_, fs) Root = ([], fs)
build (_ : current, fs) Up = (current, fs)
build (current, fs) (Cd folder) = (folder : current, fs)
build acc Ls = acc
build (current, fs@(FileSystem {getFiles = files})) (IsFile size name) =
  (current, fs {getFiles = File name current size : files})
build (current, fs@(FileSystem {getFolders = folders})) (IsDir name) =
  (current, fs {getFolders = Set.insert (name : current) folders})
build _ _ = undefined

data Line = Root | Up | Cd Text.Text | Ls | IsFile Int Text.Text | IsDir Text.Text deriving (Eq, Show)

type Parser = MP.Parsec Void Text.Text

parseLine :: Text.Text -> Line
parseLine input = case MP.runParser grammar "" input of
  Left _ -> undefined
  Right line -> line

grammar :: Parser Line
grammar =
  (Root <$ token "$ cd /")
    <|> (Up <$ token "$ cd ..")
    <|> (Cd <$ token "$ cd" <*> identifier)
    <|> (Ls <$ token "$ ls")
    <|> (IsDir <$ token "dir" <*> identifier)
    <|> (IsFile <$> number <*> identifier)
  where
    token s = L.lexeme C.space (C.string s) :: Parser Text.Text
    number = L.lexeme C.space L.decimal
    identifier = Text.pack <$> L.lexeme C.space (MP.many C.asciiChar)