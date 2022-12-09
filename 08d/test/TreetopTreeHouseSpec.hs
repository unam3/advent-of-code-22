module TreetopTreeHouseSpec where 

import Data.Map.Strict (empty)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import TreetopTreeHouse

spec :: Spec
spec = do
    describe "parseY" $ do
        it "works"
            $ shouldBe
                (parseY 1 "345")
                [((1, 1), 3), ((1, 2), 4), ((1, 3), 5)]

    --testInput <- runIO $ readFile "testInput"

    --input <- runIO $ readFile "input.txt"

    --describe "parseInput" $ do
    --    it "works"
    --        $ shouldBe
    --            (parseInput testInput)
    --            empty
