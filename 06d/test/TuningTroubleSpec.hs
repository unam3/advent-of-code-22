module TuningTroubleSpec where 

import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import TuningTrouble

spec :: Spec
spec = do
    testInput <- runIO $ readFile "testInput"

    input <- runIO $ readFile "input.txt"

    describe "identifyFirstStartOfPacketMarker" $ do
        it "works for testInput"
            $ shouldBe
                (identifyFirstStartOfPacketMarker testInput)
                (7, "jpqm")
        it "works for other test input"
            $ shouldBe
                (fmap (fst . identifyFirstStartOfPacketMarker) $
                    [
                        "bvwbjplbgvbhsrlpgdmjqwftvncz",
                        "nppdvjthqldpwncqszvftbrmjlhg",
                        "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
                        "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
                    ]
                )
                [5,6,10,11]
        it "works for input.txt"
            $ shouldBe
                (identifyFirstStartOfPacketMarker input)
                (1929, "cbls")


    describe "identifyFirstStartOfMessageMarker" $ do
        it "works for other test input"
            $ shouldBe
                (fmap (fst . identifyFirstStartOfMessageMarker) $
                    [
                        "mjqjpqmgbljsphdztnvjfqwrcgsmlb",
                        "bvwbjplbgvbhsrlpgdmjqwftvncz",
                        "nppdvjthqldpwncqszvftbrmjlhg",
                        "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
                        "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
                    ]
                )
                [19, 23, 23, 29, 26]
        it "works for input.txt"
            $ shouldBe
                (fst $ identifyFirstStartOfMessageMarker input)
                (3298)
