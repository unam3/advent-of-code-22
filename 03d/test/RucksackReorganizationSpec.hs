module RucksackReorganizationSpec where 

import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import RucksackReorganization

spec :: Spec
spec = do
    testInput <- runIO $ readFile "testInput"

    input <- runIO $ readFile "input.txt"

    describe "all lines length from input are even" $ do
        it "works for testInput"
            $ shouldBe
                (all even . fmap length $ lines testInput)
                True

        it "works for input"
            $ shouldBe
                (all even . fmap length $ lines input)
                True

    describe "divideLine" $ do
        it ""
            $ shouldBe
                (divideLine "vJrwpWtwJgWrhcsFMMfFFhFp")
                ("vJrwpWtwJgWr", "hcsFMMfFFhFp")

    --describe "parseInput" $ do
    --    it "works"
    --        $ shouldBe
    --            (parseInput testInput)
    --            [
    --                ("vJrwpWtwJgWr", "hcsFMMfFFhFp"),
    --                ("jqHRNqRjqzjGDLGL", "rsFMfFZSrLrFZsSL"),
    --                ("PmmdzqPrV", "vPwwTWBwg"),
    --                ("", ""),
    --                ("", "")
    --            ]

    describe "findItemFromBothCompartments" $ do
        it "works for first testInput line"
            $ shouldBe
                (findItemFromBothCompartments  ("vJrwpWtwJgWr", "hcsFMMfFFhFp"))
                'p'
        it "works for testInput"
            $ shouldBe
                (fmap findItemFromBothCompartments $ parseInput testInput)
                ['p', 'L', 'P', 'v', 't', 's']

    describe "getPriority" $ do
        it "works for 'Z'"
            $ shouldBe
                (getPriority 'Z')
                52

    describe "getPriority" $ do
        it "works for 'Z'"
            $ shouldBe
                (getPriority 'Z')
                52

        it "works for testInput"
            $ shouldBe
                (sum . fmap (getPriority . findItemFromBothCompartments) $ parseInput testInput)
                157

        it "works for input.txt"
            $ shouldBe
                (sum . fmap (getPriority . findItemFromBothCompartments) $ parseInput input)
                7568

