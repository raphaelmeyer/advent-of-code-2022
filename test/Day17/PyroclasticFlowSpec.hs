{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Day17.PyroclasticFlowSpec where

import qualified Data.Text as Text
import qualified Day17.PyroclasticFlow as Pyro
import Test.Hspec

exampleInput :: Text.Text
exampleInput = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

spec :: Spec
spec = do
  describe "Pyroclastic Flow" $ do
    let jets = Pyro.parseInput exampleInput

    it "should simulate falling rocks" $ do
      (length . Pyro.simulate 2022 $ jets) `shouldBe` 3068

    it "should simulate a few falling rocks" $ do
      let tower = Pyro.simulate 10 jets

      length tower `shouldBe` 17
      (tower !! 0) `shouldBe` 0b0000100
      (tower !! 3) `shouldBe` 0b1100110
      (tower !! 11) `shouldBe` 0b0010100
      (tower !! 16) `shouldBe` 0b0011110

    it "should simulate more falling rocks" $ do
      Pyro.simulate2 100 jets `shouldBe` (length . Pyro.simulate 100 $ jets)
      Pyro.simulate2 1000 jets `shouldBe` (length . Pyro.simulate 1000 $ jets)
      Pyro.simulate2 2022 jets `shouldBe` (length . Pyro.simulate 2022 $ jets)

      Pyro.simulate2 1000000000000 jets `shouldBe` 1514285714288
