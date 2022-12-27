module MonkeyInTheMiddleSpec where 

import Data.Vector ((!), fromList)
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
                ([79, 98], MultiplyBy 19, (23, 2, 3))

    describe "parseInput" $ do
        it "works"
            $ shouldBe
                (parseInput testInput)
                $ fromList [
                    ([79, 98], MultiplyBy 19, (23, 2, 3)),
                    ([54, 65, 75, 74], Add 6, (19, 2, 0)),
                    ([79, 60, 97], Sqr, (13, 1, 3)),
                    ([74], Add 3, (17, 0, 1))
                ]

    describe "inspect" $ do
        it "works for first monkey from testInput"
            $ let inputData = parseInput testInput
            in shouldBe
                (
                    inspect
                        inputData
                        0
                        ((!) inputData 0)
                )
                $ fromList [
                    ([], MultiplyBy 19, (23, 2, 3)),
                    ([54, 65, 75, 74], Add 6, (19, 2, 0)),
                    ([79, 60, 97], Sqr, (13, 1, 3)),
                    ([74, 500, 620], Add 3, (17, 0, 1))
                ]
