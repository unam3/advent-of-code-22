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
                (modelMotion [(U 4)])
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
                (modelMotion [(D 4)])
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
                (modelMotion [(R 4)])
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
                (modelMotion [(L 4)])
                (
                    ((-4, 0), (-3, 0)),
                    [
                        (0, 0),
                        (-1, 0),
                        (-2, 0),
                        (-3, 0)
                    ]
                )

    --input <- runIO $ readFile "input.txt"

    --describe "modelMotion" $ do
        --it "works for testInput"
        --    $ shouldBe
        --        ((\ (coords, tailVisitedAtLeastOnce) -> (coords, length tailVisitedAtLeastOnce))
        --            . modelMotion $ parseInput testInput
        --        )
        --        (
        --            ((2, 2), (1, 2)),
        --            13
        --        )
        --it "works as solution input.txt"
        --    $ shouldBe
        --        (length . snd . modelMotion $ parseInput input)
        --        6354

    describe "vmodelMotion" $ do
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
