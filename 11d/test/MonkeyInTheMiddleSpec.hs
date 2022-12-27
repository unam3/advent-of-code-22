module MonkeyInTheMiddleSpec where 

import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import MonkeyInTheMiddle

spec :: Spec
spec = do

    testInput <- runIO $ readFile "testInput"

    --input <- runIO $ readFile "input.txt"

    describe "parseMonkeySection" $ do
        it "parses first monkey section of testInput"
            $ shouldBe
                (parseMonkeySection $ head $ splitByEmptyLines $ lines testInput)
                (0, [79, 98], MultiplyBy 19, (23, 2, 3))

    --describe "parseInput" $ do
    --    it "works"
    --        $ shouldBe
    --            (parseInput testInput)
    --            "42"
