module MonkeyInTheMiddleSpec where 

import Data.List (foldl')
import Data.Vector ((!), fromList)
import Prelude hiding (round)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import MonkeyInTheMiddle

spec :: Spec
spec = do

    testInput <- runIO $ readFile "testInput"

    input <- runIO $ readFile "input.txt"

    --describe "parseMonkeySection" $ do
    --    it "parses first monkey section of testInput"
    --        $ shouldBe
    --            (parseMonkeySection $ head $ splitByEmptyLines $ lines testInput)
    --            (MultiplyBy 19, (23, 2, 3), 0, [79, 98])

    --describe "parseInput" $ do
    --    it "works"
    --        $ shouldBe
    --            (parseInput testInput)
    --            $ fromList [
    --                (MultiplyBy 19, (23, 2, 3), 0, [79, 98]),
    --                (Add 6, (19, 2, 0), 0, [54, 65, 75, 74]),
    --                (Sqr, (13, 1, 3), 0, [79, 60, 97]),
    --                (Add 3, (17, 0, 1), 0, [74])
    --            ]

--    describe "inspect" $ do
--        it "works for first monkey from testInput"
--            $ let inputData = parseInput testInput
--            in shouldBe
--                (
--                    inspect
--                        inputData
--                        0
--                        ((!) inputData 0)
--                )
--                $ fromList [
--                    ([], MultiplyBy 19, (23, 2, 3)),
--                    ([54, 65, 75, 74], Add 6, (19, 2, 0)),
--                    ([79, 60, 97], Sqr, (13, 1, 3)),
--                    ([74, 500, 620], Add 3, (17, 0, 1))
--                ]
--
--        it "works for second monkey from testInput"
--            $ let inputData =
--                    fromList [
--                        ([], MultiplyBy 19, (23, 2, 3)),
--                        ([54, 65, 75, 74], Add 6, (19, 2, 0)),
--                        ([79, 60, 97], Sqr, (13, 1, 3)),
--                        ([74, 500, 620], Add 3, (17, 0, 1))
--                    ]
--                  monkeyIndex = 1
--            in shouldBe
--                (
--                    inspect
--                        inputData
--                        monkeyIndex
--                        ((!) inputData monkeyIndex)
--                )
--                $ fromList [
--                    ([20, 23, 27, 26], MultiplyBy 19, (23, 2, 3)),
--                    ([], Add 6, (19, 2, 0)),
--                    ([79, 60, 97], Sqr, (13, 1, 3)),
--                    ([74, 500, 620], Add 3, (17, 0, 1))
--                ]
--
--        it "works for third monkey from testInput"
--            $ let inputData =
--                    fromList [
--                        ([20, 23, 27, 26], MultiplyBy 19, (23, 2, 3)),
--                        ([], Add 6, (19, 2, 0)),
--                        ([79, 60, 97], Sqr, (13, 1, 3)),
--                        ([74, 500, 620], Add 3, (17, 0, 1))
--                    ]
--                  monkeyIndex = 2
--            in shouldBe
--                (
--                    inspect
--                        inputData
--                        monkeyIndex
--                        ((!) inputData monkeyIndex)
--                )
--                $ fromList [
--                    ([20, 23, 27, 26], MultiplyBy 19, (23, 2, 3)),
--                    ([2080], Add 6, (19, 2, 0)),
--                    ([], Sqr, (13, 1, 3)),
--                    ([74, 500, 620, 1200, 3136], Add 3, (17, 0, 1))
--                ]
--
--        it "works for fourth monkey from testInput"
--            $ let inputData =
--                    fromList [
--                        ([20, 23, 27, 26], MultiplyBy 19, (23, 2, 3)),
--                        ([2080], Add 6, (19, 2, 0)),
--                        ([], Sqr, (13, 1, 3)),
--                        ([74, 500, 620, 1200, 3136], Add 3, (17, 0, 1))
--                    ]
--                  monkeyIndex = 3
--            in shouldBe
--                (
--                    inspect
--                        inputData
--                        monkeyIndex
--                        ((!) inputData monkeyIndex)
--                )
--                $ fromList [
--                    ([20, 23, 27, 26], MultiplyBy 19, (23, 2, 3)),
--                    ([2080, 25, 167, 207, 401, 1046], Add 6, (19, 2, 0)),
--                    ([], Sqr, (13, 1, 3)),
--                    ([], Add 3, (17, 0, 1))
--                ]

    testInput_cropped <- runIO $ readFile "testInput_cropped"

    --describe "round" $ do
        --it "works for first three monkeys from testInput"
        --    $ shouldBe
        --        (round $ parseInput testInput_cropped)
        --        $ fromList [
        --            ([20, 23, 27, 26], MultiplyBy 19, (23, 2, 3)),
        --            ([2080], Add 6, (19, 2, 0)),
        --            ([], Sqr, (13, 1, 3))
        --        ]

        --it "works for monkey \"3\" from testInput"
        --    $ let inputData =
        --            fromList [
        --                ([20, 23, 27, 26], MultiplyBy 19, (23, 2, 3)),
        --                ([2080], Add 6, (19, 2, 0)),
        --                ([], Sqr, (13, 1, 3)),
        --                ([74, 500, 620, 1200, 3136], Add 3, (17, 0, 1))
        --            ]
        --    in shouldBe
        --        (round inputData)
        --        $ fromList [
        --            ([20, 23, 27, 26], MultiplyBy 19, (23, 2, 3)),
        --            ([2080, 25, 167, 207, 401, 1046], Add 6, (19, 2, 0)),
        --            ([], Sqr, (13, 1, 3)),
        --            ([], Add 3, (17, 0, 1))
        --        ]

        --it "works for testInput first round"
        --    $ shouldBe
        --        (round dropWorryLevel $ parseInput testInput)
        --        $ fromList [
        --            (MultiplyBy 19, (23, 2, 3), 2, [20, 23, 27, 26]),
        --            (Add 6, (19, 2, 0), 4, [2080, 25, 167, 207, 401, 1046]),
        --            (Sqr, (13, 1, 3), 3, []),
        --            (Add 3, (17, 0, 1), 5, [])
        --        ]

        --it "works for testInput 20 rounds"
        --    $ shouldBe
        --        (runNRounds dropWorryLevel 19 (parseInput testInput))
        --        $ fromList [
        --            (MultiplyBy 19, (23, 2, 3), 101, [10, 12, 14, 26, 34]),
        --            (Add 6, (19, 2, 0), 95, [245, 93, 53, 199, 115]),
        --            (Sqr, (13, 1, 3), 7, []),
        --            (Add 3, (17, 0, 1), 105, [])
        --        ]

    describe "getMonkeyBusinessLevel" $ do
        it "works for testInput"
            $ shouldBe
                (getMonkeyBusinessLevel . runNRounds dropWorryLevel 19 $ parseInput testInput)
                10605

        it "works for input.txt"
            $ shouldBe
                (getMonkeyBusinessLevel . runNRounds dropWorryLevel 19 $ parseInput input)
                55944

        --it "works for part 2 80 on testInput"
        --    $ shouldBe
        --        (getMonkeyBusinessLevel . runNRounds id 100 $ parseInput testInput)
        --        10197

        --it "works for part 2 on testInput"
        --    $ shouldBe
        --        (getMonkeyBusinessLevel . runNRounds id 9999 $ parseInput testInput)
        --        2713310158

        --it "works for part 2 on input.txt"
        --    $ shouldBe
        --        (getMonkeyBusinessLevel . runNRounds id 9999 $ parseInput input)
        --        2713310158

    describe "isDivisibleBy" $ do

        it "works for 13"
            $ shouldBe
                (isDivisibleBy (13 * 5347) 13)
                True

        it "works for 17"
            $ shouldBe
                (isDivisibleBy (17 * 5347) 17)
                True

        it "works for 19"
            $ shouldBe
                (isDivisibleBy (19 * 5347) 19)
                True

        it "works for 23"
            $ shouldBe
                (isDivisibleBy (23 * 5347) 23)
                True
