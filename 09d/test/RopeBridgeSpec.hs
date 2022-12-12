module RopeBridgeSpec where 

import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import RopeBridge

spec :: Spec
spec = do
    testInput <- runIO $ readFile "testInput"

    --input <- runIO $ readFile "input.txt"

    describe "parseInput" $ do
        it "works"
            $ shouldBe
                (parseInput testInput)
                [R 4,U 4,L 3,D 1,R 4,D 1,L 5,R 2]

    describe "animate (U n)" $ do
        it "works with modelMotion"
            $ shouldBe
                (modelMotion (U 4))
                (
                    ((0, 4), (0, 3)),
                    [
                        (0, 0),
                        (0, 1),
                        (0, 2),
                        (0, 3)
                    ]
                )
