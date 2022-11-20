{-# LANGUAGE OverloadedStrings #-}

module Day01.SolutionSpec where

import Day01.Solution as Day01
import Test.Hspec

spec :: Spec
spec = do
  describe "something" $ do
    it "should answer with 42" $ do
      Day01.answer `shouldBe` (42 :: Int)
