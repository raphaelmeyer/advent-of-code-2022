{-# LANGUAGE OverloadedStrings #-}

module Day21.MonkeyMathSpec where

import qualified Data.Text as Text
import qualified Day21.MonkeyMath as M
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "root: pppw + sjmn",
    "dbpl: 5",
    "cczh: sllz + lgvd",
    "zczc: 2",
    "ptdq: humn - dvpt",
    "dvpt: 3",
    "lfqf: 4",
    "humn: 5",
    "ljgn: 2",
    "sjmn: drzm * dbpl",
    "sllz: 4",
    "pppw: cczh / lfqf",
    "lgvd: ljgn * ptdq",
    "drzm: hmdt - zczc",
    "hmdt: 32"
  ]

spec :: Spec
spec = do
  describe "Monkey Math" $ do
    let monkeys = M.parseInput exampleInput

    it "should calculate monkey math" $ do
      M.calc monkeys "root" `shouldBe` 152

    it "should find number to yell" $ do
      M.yell monkeys `shouldBe` 301

    it "should find the human" $ do
      M.findHuman monkeys "root" `shouldBe` Just ["root", "pppw", "cczh", "lgvd", "ptdq", "humn"]
      M.findHuman monkeys "sjmn" `shouldBe` Nothing
