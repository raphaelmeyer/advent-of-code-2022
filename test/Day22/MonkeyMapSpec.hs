{-# LANGUAGE OverloadedStrings #-}

module Day22.MonkeyMapSpec where

import qualified Data.Text as Text
import qualified Day22.MonkeyMap as M
import Test.Hspec

exampleInput :: [Text.Text]
exampleInput =
  [ "        ...#",
    "        .#..",
    "        #...",
    "        ....",
    "...#.......#",
    "........#...",
    "..#....#....",
    "..........#.",
    "        ...#....",
    "        .....#..",
    "        .#......",
    "        ......#.",
    "",
    "10R5L5R10L4R5L5"
  ]

spec :: Spec
spec = do
  describe "Monkey Math" $ do
    describe "Follow the flat path" $ do
      let (monkeyMap, path) = M.parseInput exampleInput
      let flat = M.getMovement monkeyMap M.Flat

      it "should calculate the final password" $ do
        M.password M.Flat monkeyMap path `shouldBe` 6032

      it "should know the final position" $ do
        M.follow flat monkeyMap path `shouldBe` (((1, 1), (3, 1)), M.FaceRight)

      it "should find the start position" $ do
        M.findStart monkeyMap `shouldBe` ((2, 0), (0, 0))

      it "should lookup quadrant positions" $ do
        M.qLookup ((2, 1), (1, 2)) monkeyMap `shouldBe` Just M.Open
        M.qLookup ((3, 2), (2, 3)) monkeyMap `shouldBe` Just M.Wall
        M.qLookup ((1, 2), (1, 2)) monkeyMap `shouldBe` Nothing

      it "should jump to next quadrants" $ do
        M.stepForward flat monkeyMap (((2, 2), (2, 1)), M.FaceRight) 5 `shouldBe` (((3, 2), (0, 1)), M.FaceRight)
        M.stepForward flat monkeyMap (((2, 1), (1, 0)), M.FaceLeft) 5 `shouldBe` (((1, 1), (0, 0)), M.FaceLeft)
        M.stepForward flat monkeyMap (((2, 1), (2, 2)), M.FaceUp) 5 `shouldBe` (((2, 0), (2, 1)), M.FaceUp)
        M.stepForward flat monkeyMap (((2, 0), (1, 3)), M.FaceDown) 5 `shouldBe` (((2, 2), (1, 0)), M.FaceDown)

      it "should wrap around empty quadrants" $ do
        M.stepForward flat monkeyMap (((3, 2), (2, 1)), M.FaceRight) 5 `shouldBe` (((2, 2), (3, 1)), M.FaceRight)
        M.stepForward flat monkeyMap (((3, 2), (2, 2)), M.FaceRight) 5 `shouldBe` (((2, 2), (0, 2)), M.FaceRight)
        M.stepForward flat monkeyMap (((2, 0), (2, 2)), M.FaceRight) 5 `shouldBe` (((2, 0), (3, 2)), M.FaceRight)

        M.stepForward flat monkeyMap (((0, 1), (3, 1)), M.FaceLeft) 5 `shouldBe` (((2, 1), (2, 1)), M.FaceLeft)
        M.stepForward flat monkeyMap (((2, 2), (1, 3)), M.FaceLeft) 5 `shouldBe` (((3, 2), (3, 3)), M.FaceLeft)
        M.stepForward flat monkeyMap (((0, 1), (2, 0)), M.FaceLeft) 5 `shouldBe` (((0, 1), (0, 0)), M.FaceLeft)

        M.stepForward flat monkeyMap (((2, 0), (2, 2)), M.FaceUp) 5 `shouldBe` (((2, 2), (2, 1)), M.FaceUp)
        M.stepForward flat monkeyMap (((0, 1), (2, 0)), M.FaceUp) 5 `shouldBe` (((0, 1), (2, 3)), M.FaceUp)
        M.stepForward flat monkeyMap (((3, 2), (2, 1)), M.FaceUp) 5 `shouldBe` (((3, 2), (2, 0)), M.FaceUp)

        M.stepForward flat monkeyMap (((3, 2), (3, 1)), M.FaceDown) 5 `shouldBe` (((3, 2), (3, 2)), M.FaceDown)
        M.stepForward flat monkeyMap (((2, 2), (0, 2)), M.FaceDown) 5 `shouldBe` (((2, 0), (0, 1)), M.FaceDown)
        M.stepForward flat monkeyMap (((0, 1), (3, 2)), M.FaceDown) 5 `shouldBe` (((0, 1), (3, 3)), M.FaceDown)

    describe "Follow the cubic path" $ do
      let (monkeyMap, path) = M.parseInput exampleInput
      let cube = M.getMovement monkeyMap M.Cube

      it "should calculate the final password" $ do
        M.password M.Cube monkeyMap path `shouldBe` 5031

      it "should know the final position" $ do
        M.follow cube monkeyMap path `shouldBe` (((1, 1), (2, 0)), M.FaceUp)

      it "should move and wrap around the cube" $ do
        M.stepForward cube monkeyMap (((2, 1), (3, 1)), M.FaceRight) 1 `shouldBe` (((3, 2), (2, 0)), M.FaceDown)
        M.stepForward cube monkeyMap (((2, 2), (2, 3)), M.FaceDown) 1 `shouldBe` (((0, 1), (1, 3)), M.FaceUp)
        M.stepForward cube monkeyMap (((1, 1), (2, 0)), M.FaceUp) 1 `shouldBe` (((1, 1), (2, 0)), M.FaceUp)

    describe "Parse the input" $ do
      let (board, description) = M.splitInput exampleInput

      it "should parse the input" $ do
        let (monkeyMap, path) = M.parseInput exampleInput

        M.getSideLen monkeyMap `shouldBe` 4
        M.quadrant monkeyMap (0, 0) `shouldBe` M.Empty
        head path `shouldBe` M.Forward 10

      it "should split the input" $ do
        length board `shouldBe` 12
        board !! 6 `shouldBe` "..#....#...."

        description `shouldBe` "10R5L5R10L4R5L5"

      it "should parse the map" $ do
        let dim = M.dimensions board
        dim `shouldBe` (4, 3)

        let monkeyMap = M.parseMap dim board

        M.getSideLen monkeyMap `shouldBe` 4

        M.quadrant monkeyMap (0, 0) `shouldBe` M.Empty
        M.quadrant monkeyMap (1, 0) `shouldBe` M.Empty
        M.quadrant monkeyMap (3, 0) `shouldBe` M.Empty
        M.quadrant monkeyMap (3, 1) `shouldBe` M.Empty
        M.quadrant monkeyMap (0, 2) `shouldBe` M.Empty
        M.quadrant monkeyMap (1, 2) `shouldBe` M.Empty

        case M.quadrant monkeyMap (2, 0) of
          M.Empty -> expectationFailure "should not be empty"
          M.Face grid -> do
            grid `M.at` (0, 1) `shouldBe` M.Open
            grid `M.at` (1, 1) `shouldBe` M.Wall

        case M.quadrant monkeyMap (3, 2) of
          M.Empty -> expectationFailure "should not be empty"
          M.Face grid -> do
            grid `M.at` (2, 3) `shouldBe` M.Wall
            grid `M.at` (3, 3) `shouldBe` M.Open

      it "should parse the path" $ do
        M.parsePath description
          `shouldBe` [ M.Forward 10,
                       M.TurnRight,
                       M.Forward 5,
                       M.TurnLeft,
                       M.Forward 5,
                       M.TurnRight,
                       M.Forward 10,
                       M.TurnLeft,
                       M.Forward 4,
                       M.TurnRight,
                       M.Forward 5,
                       M.TurnLeft,
                       M.Forward 5
                     ]
