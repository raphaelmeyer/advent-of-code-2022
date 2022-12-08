module Day08.TreeHouse where

import qualified Data.Char as Char
import Data.Functor ((<&>))
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Vector as Vector

-- runner
run :: IO ()
run = do
  input <- readInput "data/day-08.txt"
  let grid = parseInput input
  let visible = countVisible grid

  let forest = makeForest grid
  let score = highestScenicScore forest

  putStrLn ""
  putStrLn "# Day 08 #"
  putStrLn ""
  putStrLn $ "Part I : " ++ show visible
  putStrLn $ "Part II : " ++ show score

-- part one

type Row = [Int]

type Grid = [Row]

countVisible :: Grid -> Int
countVisible grid = sum . map (length . filter id) $ visible
  where
    visible = List.zipWith (List.zipWith (||)) horizontal vertical
    vertical = List.zipWith (List.zipWith (||)) (lookFromTop grid) (lookFromBottom grid)
    horizontal = List.zipWith (List.zipWith (||)) (lookFromLeft grid) (lookFromRight grid)

lookFromLeft :: Grid -> [[Bool]]
lookFromLeft = map (reverse . fst . foldl lookHorizontal ([], -1))

lookFromRight :: Grid -> [[Bool]]
lookFromRight = map (fst . foldr (flip lookHorizontal) ([], -1))

lookHorizontal :: ([Bool], Int) -> Int -> ([Bool], Int)
lookHorizontal (visible, cur) tree
  | cur < tree = (True : visible, tree)
  | otherwise = (False : visible, cur)

lookFromTop :: Grid -> [[Bool]]
lookFromTop = reverse . fst . foldl lookVertical ([], repeat (-1 :: Int))

lookFromBottom :: Grid -> [[Bool]]
lookFromBottom = fst . foldr (flip lookVertical) ([], repeat (-1 :: Int))

lookVertical :: ([[Bool]], [Int]) -> Row -> ([[Bool]], [Int])
lookVertical (visible, curs) trees = (reverse looked : visible, reverse nexts)
  where
    (looked, nexts) = foldl look ([], []) $ zip curs trees
    look (v, next) (cur, tree)
      | cur < tree = (True : v, tree : next)
      | otherwise = (False : v, cur : next)

-- part two

type Forest = Vector.Vector (Vector.Vector Int)

type Pos = (Int, Int)

makeForest :: Grid -> Forest
makeForest grid = Vector.fromList (map Vector.fromList grid)

highestScenicScore :: Forest -> Int
highestScenicScore forest = foldl scoreRow 0 rows
  where
    rows = [1 .. (Vector.length forest - 2)]
    scoreRow rowHighest j = foldl scoreCol rowHighest cols
      where
        cols = [1 .. (Vector.length ((Vector.!) forest j) - 2)]
        scoreCol colHighest i = max colHighest (scenicScore forest (i, j))

scenicScore :: Forest -> Pos -> Int
scenicScore forest (i, j) = case at (i, j) forest of
  Just me -> product . map (\dir -> count dir me (i, j) forest) $ [left, right, up, down]
  Nothing -> 0

count :: (Pos -> Pos) -> Int -> Pos -> Forest -> Int
count dir me pos forest = case at next forest of
  Just tree -> if tree < me then 1 + count dir me next forest else 1
  Nothing -> 0
  where
    next = dir pos

up :: Pos -> Pos
up (i, j) = (i, j - 1)

down :: Pos -> Pos
down (i, j) = (i, j + 1)

left :: Pos -> Pos
left (i, j) = (i - 1, j)

right :: Pos -> Pos
right (i, j) = (i + 1, j)

at :: Pos -> Forest -> Maybe Int
at (i, j) forest = case (Vector.!?) forest j of
  Just trees -> (Vector.!?) trees i
  Nothing -> Nothing

-- parse input

readInput :: String -> IO [Text.Text]
readInput filename = readFile filename <&> Text.lines . Text.pack

parseInput :: [Text.Text] -> Grid
parseInput = map parseLine

parseLine :: Text.Text -> [Int]
parseLine = Text.foldr (\t ts -> Char.digitToInt t : ts) []
