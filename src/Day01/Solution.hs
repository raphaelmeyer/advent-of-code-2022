module Day01.Solution where

run :: IO ()
run = do
  putStrLn ""
  putStrLn "# Day 01 #"
  putStrLn ""
  putStrLn $ "Part I : " ++ show answer
  putStrLn $ "Part II : " ++ show (answer + 1)

answer :: Int
answer = 42
