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

    describe "f" $ do
        it "works"
            $ shouldBe
                42
                42
