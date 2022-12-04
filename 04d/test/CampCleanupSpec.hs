module CampCleanupSpec where 

import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import CampCleanup

spec :: Spec
spec = do
    testInput <- runIO $ readFile "testInput"

    --input <- runIO $ readFile "input.txt"

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
        it "works"
            $ shouldBe
                (fmap isSectionContainAnother $ parseInput testInput)
                [
                    False,
                    False,
                    False,
                    True,
                    True,
                    False
                ]
                
