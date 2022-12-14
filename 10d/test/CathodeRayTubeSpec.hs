module CathodeRayTubeSpec where 

import Control.Monad ((>=>))
import Data.Functor ((<&>))
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import CathodeRayTube

spec :: Spec
spec = do
    testInput <- runIO $ readFile "testInput"

    describe "parseInstruction" $ do
        it "works for noop"
            $ shouldBe
                (parseInstruction "noop")
                (Right Noop)

        it "works for valid addx input string"
            $ shouldBe
                (parseInstruction "addx -12")
                (Right $ Addx (-12))
        
        it "works for not supported instruction"
            $ let notSupportedInstruction = "pluh -12"
                in shouldBe
                (parseInstruction notSupportedInstruction)
                (Left $ "parseInstruction non-matched input: " ++ show notSupportedInstruction)
        
        it "works for instruction with wrong value"
            $ let notSupportedInstruction = "pluh meh"
                in shouldBe
                (parseInstruction notSupportedInstruction)
                (Left $ "parseInstruction non-matched input: " ++ show notSupportedInstruction)

    describe "parseInput" $ do
        it "works for testInput"
            $ shouldBe
                (parseInput testInput)
                (Right [Noop, Addx 3, Addx (-5)])

    describe "execute" $ do
        it "works for noop"
            $ shouldBe
                (execute (1, 0) Noop)
                (1, 1)

        it "works for addx"
            $ shouldBe
                (execute (1, 0) $ Addx (-2))
                (-1, 2)

    describe "executeInstructions" $ do
        it "works for testInput"
            $ shouldBe
                (parseInput testInput >>= executeInstructions)
                (Right (-1, 5))

    describe "updateF" $ do
        it "works"
            $ shouldBe
                (updateF (1, Just 12) [] (1, Nothing))
                [(1, Just 12)]

    describe "update" $ do
        it "works"
            $ shouldBe
                (update [(1, Nothing), (3, Nothing), (4, Nothing)] (3, Just 12))
                [(1, Nothing), (3, Just 12), (4, Nothing)]

    describe "execute'" $ do
        it "works for noop"
            $ shouldBe
                (execute' (12, 0, [(1, Nothing)]) Noop)
                (12, 1, [(1, Just 12)])

        it "works for addx"
            $ shouldBe
                (execute' (12, 0, [(1, Nothing)]) (Addx 3))
                (15, 2, [(1, Just 12)])


    testInput2 <- runIO $ readFile "testInput2"

    describe "executeInstructions'" $ do
        it "works for testInput2"
            $ shouldBe
                (parseInput testInput2 >>= executeInstructions' (zip [20, 60, 100, 140, 180, 220] $ repeat Nothing))
                (Right (17,240,[(20,Just 420),(60,Just 1140),(100,Just 1800),(140,Just 2940),(180,Just 2880),(220,Just 3960)]))


    input <- runIO $ readFile "input.txt"

    describe "getSignalStrengthFor" $ do
        it "works for testInput2"
            $ shouldBe
                (parseInput testInput2 <&> getSignalStrengthFor [20, 60, 100, 140, 180, 220])
                (Right 13140)

        it "works for testInput2"
            $ shouldBe
                (parseInput input <&> getSignalStrengthFor [20, 60, 100, 140, 180, 220])
                (Right 14760)

    describe "executeII" $ do
        it "works for Noop"
            $ shouldBe
                (
                    executeII
                        2
                        (1, 0, [False])
                        Noop
                )
                (
                    1,
                    1,
                    [False, True]
                )

        it "works for Addx -11"
            $ shouldBe
                (
                    executeII
                        5
                        (16, 3, [])
                        (Addx (-11))
                )
                (
                    5,
                    5,
                    [False, False]
                )

    describe "mapCycleNumbers" $ do
        it "works for testInput"
            $ shouldBe
                (parseInput testInput >>= Right . mapCycleNumbers)
                (Right [(1,Noop),(2,Addx 3),(3,Addx 3),(4,Addx (-5)),(5,Addx (-5))])

    describe "adjustCycleNumber" $ do
        it "works"
            $ shouldBe
                (fmap adjustCycleNumber [1, 40, 41, 80, 81, 119])
                [1, 40, 1, 40, 1, 39]

    describe "executeInstructionsII" $ do
        it "works for testInput2 short listing"
            $ shouldBe
                (parseInput testInput2 >>= executeInstructionsII 21)
                (Right (
                    20,
                    21,
                    fmap pixelToBool "##..##..##..##..##..#"
                ))

        it "works for testInput2 first row (40)"
            $ shouldBe
                (parseInput testInput2
                    -- >>= ( \ instructions ->
                    --     executeInstructionsII 40 instructions
                    --         >>= (\ (_, cycleNumber, crtState) -> Right (cycleNumber, crtState))
                    -- )
                    >>= (executeInstructionsII 40
                        >=> (\ (_, cycleNumber, crtState) -> Right (cycleNumber, crtState))
                    )
                )
                (Right (
                    40,
                    fmap pixelToBool "##..##..##..##..##..##..##..##..##..##.."
                ))

        --it "works for testInput2 part"
        --    $ shouldBe
        --        (parseInput testInput2 >>= executeInstructionsII 42 >>= pure . fmapBoolToPixel)
        --        (Right (
        --            0,
        --            45,
        --            "##..##..##..##..##..##..##..##..##..##..###."
        --        ))

        --it "works for testInput2 part"
        --    $ shouldBe
        --        (parseInput testInput2
        --            >>= executeInstructionsII 44
        --                >>= (\ (_, _, crtState) -> pure . fmap (fmap boolToPixel) $ splitCRTStateInSix crtState)
        --        )
        --        (Right [
        --            "##..##..##..##..##..##..##..##..##..##..",
        --            "###.",
        --            "",
        --            "",
        --            "",
        --            ""
        --        ])

        it "works for testInput2"
            $ shouldBe
                (parseInput testInput2
                    >>= executeInstructionsII 240
                        >>= (\ (_, _, crtState) -> pure . fmap (fmap boolToPixel) $ splitCRTStateInSix crtState)
                )
                (Right [
                    "##..##..##..##..##..##..##..##..##..##..",
                    "###...###...###...###...###...###...###.",
                    "####....####....####....####....####....",
                    "#####.....#####.....#####.....#####.....",
                    "######......######......######......####",
                    "#######.......#######.......#######....."
                ])

        it "works for input"
            $ shouldBe
                (parseInput input
                    >>= executeInstructionsII 240
                        >>= (\ (_, _, crtState) -> pure . fmap (fmap boolToPixel) $ splitCRTStateInSix crtState)
                )
                (Right [])
