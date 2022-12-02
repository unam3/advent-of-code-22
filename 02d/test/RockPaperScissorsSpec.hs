module RockPaperScissorsSpec where 

import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import RockPaperScissors

spec :: Spec
spec = do
    testInput <- runIO $ readFile "testInput"

    input <- runIO $ readFile "input.txt"

    describe "parseInput" $ do
        it "works"
            $ shouldBe
                (parseInput testInput)
                [(A, Y), (B, X), (C, Z)]

    describe "playRound" $ do
        it "works for testInput"
            $ shouldBe
                (fmap playRound $ parseInput testInput)
                [8, 1, 6]
        it "wiht getTotalScore works for testInput"
            $ shouldBe
                (getTotalScore $ fmap playRound $ parseInput testInput)
                15
        it "wiht getTotalScore works for input.txt"
            $ shouldBe
                (getTotalScore $ fmap playRound $ parseInput input)
                11603
