module RopeBridgeSpec where 

import Data.Vector (fromList)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import RopeBridge

spec :: Spec
spec = do
    testInput <- runIO $ readFile "testInput"

    describe "parseInput" $ do
        it "works"
            $ shouldBe
                (parseInput testInput)
                [R 4,U 4,L 3,D 1,R 4,D 1,L 5,R 2]

    describe "animate" $ do
        it "animate (U n) works with modelMotion"
            $ shouldBe
                (modelMotion [U 4])
                (
                    ((0, 4), (0, 3)),
                    [
                        (0, 0),
                        (0, 1),
                        (0, 2),
                        (0, 3)
                    ]
                )
        it "animate (D n) works with modelMotion"
            $ shouldBe
                (modelMotion [D 4])
                (
                    ((0, -4), (0, -3)),
                    [
                        (0, 0),
                        (0, -1),
                        (0, -2),
                        (0, -3)
                    ]
                )
        it "animate (R n) works with modelMotion"
            $ shouldBe
                (modelMotion [R 4])
                (
                    ((4, 0), (3, 0)),
                    [
                        (0, 0),
                        (1, 0),
                        (2, 0),
                        (3, 0)
                    ]
                )
        it "animate (L n) works with modelMotion"
            $ shouldBe
                (modelMotion [L 4])
                (
                    ((-4, 0), (-3, 0)),
                    [
                        (0, 0),
                        (-1, 0),
                        (-2, 0),
                        (-3, 0)
                    ]
                )

    input <- runIO $ readFile "input.txt"

    --describe "modelMotion" $ do
    --    it "works for testInput"
    --        $ shouldBe
    --            ((\ (coords, tailVisitedAtLeastOnce) -> (coords, length tailVisitedAtLeastOnce))
    --                . modelMotion $ parseInput testInput
    --            )
    --            (
    --                ((2, 2), (1, 2)),
    --                13
    --            )
    --    it "works as solution input.txt"
    --        $ shouldBe
    --            (length . snd . modelMotion $ parseInput input)
    --            6354

    --describe "getModifier" $ do
    --    it "works for L"
    --        $ let rt = (0, -2)
    --            in shouldBe
    --                ((getModifier (-2, -2) 2 rt 0)
    --                    rt
    --                )
    --                (-1, -2)

    --    it "works for R"
    --        $ let rt = (-4, -2)
    --            in shouldBe
    --                ((getModifier (-2, -2) 2 rt 0)
    --                    rt
    --                )
    --                (-3, -2)

    --    it "works for U"
    --        $ let rt = (-2, -4)
    --            in shouldBe
    --                ((getModifier (-2, -2) 0 rt 2)
    --                    rt
    --                )
    --                (-2, -3)

    --    it "works for D"
    --        $ let rt = (-2, 0)
    --            in shouldBe
    --                ((getModifier (-2, -2) 0 rt 2)
    --                    rt
    --                )
    --                (-2, -1)

--        it "works for ↖ vertical rectangle"
--            $ let rt = (-1, -4)
--                in shouldBe
--                    ((getModifier (-2, -2) 1 rt 2)
--                        rt
--                    )
--                    (-2, -3)
--
--        it "works for ↖ horizontal rectangle"
--            $ let rt = (0, -3)
--                in shouldBe
--                    ((getModifier (-2, -2) 2 rt 1)
--                        rt
--                    )
--                    (-1, -2)
--
--        it "works for ↙ vertical rectangle"
--            $ let rt = (-1, 0)
--                in shouldBe
--                    ((getModifier (-2, -2) 1 rt 2)
--                        rt
--                    )
--                    (-2, -1)
--
--        it "works for ↙ horizontal rectangle"
--            $ let rt = (0, -1)
--                in shouldBe
--                    ((getModifier (-2, -2) 2 rt 1)
--                        rt
--                    )
--                    (-1, -2)
--
--        it "works for ↗ vertical rectangle"
--            $ let rt = (-3, -4)
--                in shouldBe
--                    ((getModifier (-2, -2) 1 rt 2)
--                        rt
--                    )
--                    (-2, -3)
--
--        it "works for ↗ horizontal rectangle"
--            $ let rt = (-4, -3)
--                in shouldBe
--                    ((getModifier (-2, -2) 2 rt 1)
--                        rt
--                    )
--                    (-3, -2)
--
--        it "works for ↘ vertical rectangle"
--            $ let rt = (-3, 0)
--                in shouldBe
--                    ((getModifier (-2, -2) 1 rt 2)
--                        rt
--                    )
--                    (-2, -1)
--
--        it "works for ↘ horizontal rectangle"
--            $ let rt = (-4, -1)
--                in shouldBe
--                    ((getModifier (-2, -2) 2 rt 1)
--                        rt
--                    )
--                    (-3, -2)

    testInput2 <- runIO $ readFile "testInput2"

    describe "vmodelMotion/vanimate'" $ do
        it "works for U 1"
            $ shouldBe
                (vmodelMotion [U 1])
                (
                    fromList [(0,1), (0,0), (0,0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0)],
                    [(0,0)]
                )
        it "works for U 2"
            $ shouldBe
                (vmodelMotion [U 2])
                (
                    fromList [(0,2), (0,1), (0,0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0)],
                    [(0,0)]
                )
        it "works for U 4"
            $ shouldBe
                (vmodelMotion [U 4])
                (
                    fromList [(0,4), (0,3), (0,2), (0, 1), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0)],
                    [(0,0)]
                )
        it "works for D 4"
            $ shouldBe
                (vmodelMotion [D 4])
                (
                    fromList [(0,-4), (0,-3), (0,-2), (0,-1), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0)],
                    [(0,0)]
                )
        it "works for L 4"
            $ shouldBe
                (vmodelMotion [L 4])
                (
                    fromList [(-4,0), (-3,0), (-2,0), (-1,0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0)],
                    [(0,0)]
                )
        it "works for R 4"
            $ shouldBe
                (vmodelMotion [R 4])
                (
                    fromList [(4,0), (3,0), (2,0), (1,0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0)],
                    [(0,0)]
                )
        it "works for testInput2"
            $ shouldBe
                (length $ snd $ vmodelMotion $ parseInput testInput2)
                36

        it "works for input"
            $ shouldBe
                (length $ snd $ vmodelMotion $ parseInput input)
                2651

    describe "visualize must help to visually compare step results" $ do
        it "works for testInput2"
            $ shouldBe
                (visualize $ vmodelMotion $ parseInput testInput2)
                "H\n1\n2\n3\n4\n5\n6\n7\n8\n9\n"
