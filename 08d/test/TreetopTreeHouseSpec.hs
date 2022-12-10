module TreetopTreeHouseSpec where 

import Data.Map.Strict (fromList)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import TreetopTreeHouse

spec :: Spec
spec = do
    describe "parseX" $ do
        it "works"
            $ shouldBe
                (parseX 1 "345")
                [((1, 1), 3), ((2, 1), 4), ((3, 1), 5)]

    describe "parseInput" $ do
        it "works"
            $ shouldBe
                (parseInput "345\n789")
                $ fromList [
                    ((1, 1), 3), ((2, 1), 4), ((3, 1), 5),
                    ((1, 2), 7), ((2, 2), 8), ((3, 2), 9)
                ]

    testInput <- runIO $ readFile "testInput"

    describe "countTreesAroundEdge" $ do
        it "works for testInput"
            $ shouldBe
                (countTreesAroundEdge $ parseInput testInput)
                16

    describe "isTreeVisibleFromLeft" $ do
        it "all are lower"
            $ shouldBe
                (isTreeVisibleFromLeft (fromList [((1, 1), 3), ((2, 1), 4), ((3, 1), 5)]) (3,1) 5)
                True

        it ""
            $ shouldBe
                (isTreeVisibleFromLeft (fromList [((1, 1), 5), ((2, 1), 4), ((3, 1), 5)]) (3,1) 5)
                False

        it "works for testInput top-left 5"
            $ shouldBe
                (isTreeVisibleFromLeft (parseInput testInput) (2,2) 5)
                True

    describe "isTreeVisibleFromRight" $ do
        it "all are lower"
            $ shouldBe
                (isTreeVisibleFromRight (fromList [((1, 1), 5), ((2, 1), 4), ((3, 1), 2)]) (1,1) 5)
                True
        it ""
            $ shouldBe
                (isTreeVisibleFromRight (fromList [((1, 1), 4), ((2, 1), 4), ((3, 1), 1)]) (1,1) 4)
                False

    describe "isTreeVisibleFromTop" $ do
        it "all are lower"
            $ shouldBe
                (isTreeVisibleFromTop (fromList [((1, 1), 3), ((1, 2), 4), ((1, 3), 5)]) (1,3) 5)
                True
        it ""
            $ shouldBe
                (isTreeVisibleFromTop (fromList [((1, 1), 4), ((1, 2), 4), ((1, 3), 5)]) (1,3) 3)
                False

    describe "isTreeVisibleFromBottom" $ do
        it "all are lower"
            $ shouldBe
                (isTreeVisibleFromBottom (fromList [((1, 1), 5), ((1, 2), 4), ((1, 3), 3)]) (1,1) 5)
                True
        it ""
            $ shouldBe
                (isTreeVisibleFromBottom (fromList [((1, 1), 4), ((1, 2), 4), ((1, 3), 5)]) (1,1) 4)
                False


    describe "isInteriorTreeVisible" $ do
        it "works for testInput top-left 5 (visible from left and top)"
            $ shouldBe
                (isInteriorTreeVisible (parseInput testInput) (2, 2))
                True
        it "works for testInput left-middle 5 (visible from the right)"
            $ shouldBe
                (isInteriorTreeVisible (parseInput testInput) (2, 3))
                True
        it "works for testInput bottom-middle 5 (visible from the bottom)"
            $ shouldBe
                (isInteriorTreeVisible (parseInput testInput) (3, 4))
                True

    describe "findInteriorVisibleTrees" $ do
        it "works for testInput"
            $ shouldBe
                (findInteriorVisibleTrees $ parseInput testInput)
                5

    --input <- runIO $ readFile "input.txt"

    describe "countTreesVisibleFromOutside" $ do
        it "works for testInput"
            $ shouldBe
                (countTreesVisibleFromOutside $ parseInput testInput)
                21
    

