module CalorieCountingSpec where 

import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import CalorieCounting

spec :: Spec
spec = do
    testInput <- runIO $ readFile "testInput"

    --input <- runIO $ readFile "input.txt"

    describe "parseInput" $ do
        it "works"
            $ shouldBe
                (parseInput testInput)
                []

    describe "f" $ do
        it "works"
            $ shouldBe
                42
                42
