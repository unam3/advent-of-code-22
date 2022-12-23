module CathodeRayTubeSpec where 

import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import CathodeRayTube

spec :: Spec
spec = do
    testInput <- runIO $ readFile "testInput"

    --input <- runIO $ readFile "input.txt"


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
