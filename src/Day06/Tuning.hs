{-# LANGUAGE OverloadedStrings #-}

module Day06.Tuning where

import Data.Functor ((<&>))
import qualified Data.List as List
import qualified Data.Text as Text

run :: IO ()
run = do
  input <- readInput "data/day-06.txt"
  let line = head input
  let packet = packetMarker line
  let message = messageMarker line

  putStrLn ""
  putStrLn "# Day 06 #"
  putStrLn ""
  putStrLn $ "Part I : " ++ show packet
  putStrLn $ "Part II : " ++ show message

readInput :: String -> IO [Text.Text]
readInput filename = readFile filename <&> Text.lines . Text.pack

packetMarker :: Text.Text -> Int
packetMarker = startMarker 4

messageMarker :: Text.Text -> Int
messageMarker = startMarker 14

startMarker :: Int -> Text.Text -> Int
startMarker len input = case find' input of
  Just n -> n + len
  Nothing -> -1
  where
    find' = List.findIndex ((== len) . length . List.nub . Text.unpack . Text.take len) . Text.tails
