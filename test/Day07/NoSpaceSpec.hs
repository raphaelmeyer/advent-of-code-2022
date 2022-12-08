{-# LANGUAGE OverloadedStrings #-}

module Day07.NoSpaceSpec where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Day07.NoSpace as NoSpace
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "$ cd /",
    "$ ls",
    "dir a",
    "14848514 b.txt",
    "8504156 c.dat",
    "dir d",
    "$ cd a",
    "$ ls",
    "dir e",
    "29116 f",
    "2557 g",
    "62596 h.lst",
    "$ cd e",
    "$ ls",
    "584 i",
    "$ cd ..",
    "$ cd ..",
    "$ cd d",
    "$ ls",
    "4060174 j",
    "8033020 d.log",
    "5626152 d.ext",
    "7214296 k"
  ]

spec :: Spec
spec = do
  describe "No Space Left On Device" $ do
    let fs = NoSpace.parseInput exampleInput

    it "should parse the terminal output" $ do
      let folders = NoSpace.getFolders fs
      Set.size folders `shouldBe` 4
      folders `shouldSatisfy` Set.member []
      folders `shouldSatisfy` Set.member ["a"]
      folders `shouldSatisfy` Set.member ["e", "a"]
      folders `shouldSatisfy` Set.member ["d"]

      let files = NoSpace.getFiles fs
      length files `shouldBe` 10
      files `shouldContain` [NoSpace.File "i" ["e", "a"] 584]
      files `shouldContain` [NoSpace.File "h.lst" ["a"] 62596]
      files `shouldContain` [NoSpace.File "d.ext" ["d"] 5626152]

    it "should parse a line in the terminal output" $ do
      NoSpace.parseLine "$ cd /" `shouldBe` NoSpace.Root
      NoSpace.parseLine "$ cd .." `shouldBe` NoSpace.Up
      NoSpace.parseLine "$ cd asdf" `shouldBe` NoSpace.Cd "asdf"
      NoSpace.parseLine "$ ls" `shouldBe` NoSpace.Ls
      NoSpace.parseLine "1234 poiu.qwer" `shouldBe` NoSpace.IsFile 1234 "poiu.qwer"
      NoSpace.parseLine "dir qwer" `shouldBe` NoSpace.IsDir "qwer"

    it "should determine the total size of each directory" $ do
      let sizes = NoSpace.folderSizes fs
      sizes `shouldContain` [584]
      sizes `shouldContain` [94853]
      sizes `shouldContain` [24933642]
      sizes `shouldContain` [48381165]

    it "should sum small folder sizes" $ do
      NoSpace.smallFolderSum fs `shouldBe` 95437

    it "should find directory to delete" $ do
      NoSpace.smallestBigEnough fs `shouldBe` 24933642
