module SupplyStacksSpec where 

import Data.Map.Strict (fromList)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import SupplyStacks

spec :: Spec
spec = do

    splitByEmptyLinesTestData <- runIO $ readFile "splitByEmptyLinesTestData"

    describe "splitByEmptyLines" $ do
        it "works"
            $ shouldBe
                (splitByEmptyLines $ lines splitByEmptyLinesTestData)
                [["12","33"],["34","44"],["55"]]

    testInput <- runIO $ readFile "testInput"

    describe "preprocessStackOfCrates" $ do
        it "works"
            $ shouldBe
                (preprocessStackOfCrates $ head $ splitByEmptyLines $ lines testInput)
                ["1ZN","2MCD","3P"]

    describe "parseStackOfCrates" $ do
        it "works"
            $ shouldBe
                (parseStackOfCrates "1ZN")
                ("1", "NZ")

    describe "parseRearrangementProcedure" $ do
        it "works"
            $ shouldBe
                (parseRearrangementProcedure "move 3 from 5 to 2")
                $ RearrangementProcedure 3 "5" "2" 

    describe "parseInput" $ do
        it "works"
            $ shouldBe
                (parseInput testInput)
                (
                    fromList [("1","NZ"),("2","DCM"),("3","P")],
                    [
                        RearrangementProcedure 1 "2" "1",
                        RearrangementProcedure 3 "1" "3",
                        RearrangementProcedure 2 "2" "1",
                        RearrangementProcedure 1 "1" "2"
                    ]
                )

    describe "rearrange" $ do
        it "works for testInput"
            $ shouldBe
                (uncurry (rearrange reverse) $ parseInput testInput)
                (
                    fromList [("1","C"),("2","M"),("3","ZNDP")]
                )

    input <- runIO $ readFile "input.txt"

    describe "getTopCrates" $ do
        it "works for input.txt"
            $ shouldBe
                (getTopCrates . uncurry (rearrange reverse) $ parseInput input)
                "MQSHJMWNH"
        it "works for input.txt in part 2"
            $ shouldBe
                (getTopCrates . uncurry (rearrange id) $ parseInput input)
                "LLWJRBHVZ"
