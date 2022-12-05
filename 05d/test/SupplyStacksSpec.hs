module SupplyStacksSpec where 

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

    --input <- runIO $ readFile "input.txt"

    --describe "parseInput" $ do
    --    it "works"
    --        $ shouldBe
    --            (parseInput testInput)
    --            "42"

    --describe "f" $ do
    --    it "works"
    --        $ shouldBe
    --            42
    --            42
