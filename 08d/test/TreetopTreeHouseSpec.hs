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

    --input <- runIO $ readFile "input.txt"

