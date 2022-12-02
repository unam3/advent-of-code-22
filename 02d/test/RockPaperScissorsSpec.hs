module RockPaperScissorsSpec where 

import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import RockPaperScissors

spec :: Spec
spec = do
    testInput <- runIO $ readFile "testInput"

    --input <- runIO $ readFile "input.txt"

    describe "parseInput" $ do
        it "works"
            $ shouldBe
                (parseInput testInput)
                [(A, Y), (B, X), (C, Z)]

    describe "f" $ do
        it "works"
            $ shouldBe
                42
                42
