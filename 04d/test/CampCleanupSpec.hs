module CampCleanupSpec where 

import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import CampCleanup

spec :: Spec
spec = do
    testInput <- runIO $ readFile "testInput"

    input <- runIO $ readFile "input.txt"

    describe "parseRange" $ do
        it "works"
            $ shouldBe
                (parseRange "12-34")
                (12, 34)

    describe "parsePair" $ do
        it "works"
            $ shouldBe
                (parsePair "2-4,6-8")
                ((2, 4), (6,8))

    describe "parseInput" $ do
        it "works"
            $ shouldBe
                (last $ parseInput testInput)
                ((2,6), (4,8))

    describe "isSectionContainAnother" $ do
        it "works for testInput"
            $ shouldBe
                (isSectionContainAnother <$> parseInput testInput)
                [
                    False,
                    False,
                    False,
                    True,
                    True,
                    False
                ]

        it "works for 4-6, 4-5"
            $ shouldBe
                (isSectionContainAnother ((4,6), (4,5)))
                True

        it "works for 4-5, 4-6"
            $ shouldBe
                (isSectionContainAnother ((4,5), (4,6)))
                True

        it "works for 5-6, 4-6"
            $ shouldBe
                (isSectionContainAnother ((5,6), (4,6)))
                True

        it "works for 5-5, 4-6"
            $ shouldBe
                (isSectionContainAnother ((5,5), (4,6)))
                True

        it "works for 4-6, 5-5"
            $ shouldBe
                (isSectionContainAnother ((4,6), (5,5)))
                True

        it "works for 5-5, 5-5"
            $ shouldBe
                (isSectionContainAnother ((5,5), (5,5)))
                True

        it "answer the question: in how many assignment pairs does one range fully contain the other?"
            $ shouldBe
                (length $ filter (== True) $ fmap isSectionContainAnother $ parseInput input)
                584
