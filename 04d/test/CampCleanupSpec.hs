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

    --describe "f" $ do
    --    it "works"
    --        $ shouldBe
    --            42
    --            42
