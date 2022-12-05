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
                ('1', "ZN")

    describe "parseRearrangementProcedure" $ do
        it "works"
            $ shouldBe
                (parseRearrangementProcedure "move 3 from 5 to 2")
                $ RearrangementProcedure "3" 5 2 

    --input <- runIO $ readFile "input.txt"

    describe "parseInput" $ do
        it "works"
            $ shouldBe
                (parseInput testInput)
                (
                    fromList [('1',"ZN"),('2',"MCD"),('3',"P")],
                    [
                        RearrangementProcedure "1" 2 1,
                        RearrangementProcedure "3" 1 3,
                        RearrangementProcedure "2" 2 1,
                        RearrangementProcedure "1" 1 2
                    ]
                )

    --describe "f" $ do
    --    it "works"
    --        $ shouldBe
    --            42
    --            42
