module TuningTroubleSpec where 

import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import TuningTrouble

spec :: Spec
spec = do
    testInput <- runIO $ readFile "testInput"

    --input <- runIO $ readFile "input.txt"

    describe "parseInput" $ do
        it "works"
            $ shouldBe
                (parseInput testInput)
                "42"

    describe "f" $ do
        it "works"
            $ shouldBe
                42
                42
