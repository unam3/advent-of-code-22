module TuningTroubleSpec where 

import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import TuningTrouble

spec :: Spec
spec = do
    testInput <- runIO $ readFile "testInput"

    --input <- runIO $ readFile "input.txt"

    describe "identifyFirstPosition" $ do
        it "works for testInput"
            $ shouldBe
                (identifyFirstPosition testInput)
                (7, "jpqm")
        it "works for other test input"
            $ shouldBe
                (fmap (fst . identifyFirstPosition) $
                    [
                        "bvwbjplbgvbhsrlpgdmjqwftvncz",
                        "nppdvjthqldpwncqszvftbrmjlhg",
                        "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
                        "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
                    ]
                )
                [5,6,10,11]
