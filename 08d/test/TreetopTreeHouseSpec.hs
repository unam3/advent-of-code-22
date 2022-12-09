module TreetopTreeHouseSpec where 

import Data.Map.Strict (empty)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import TreetopTreeHouse

spec :: Spec
spec = do
    describe "parseY" $ do
        it "works"
            $ shouldBe
                (parseY "345")
                [(1, 3), (2, 4), (3, 5)]

    --testInput <- runIO $ readFile "testInput"

    --input <- runIO $ readFile "input.txt"

    --describe "parseInput" $ do
    --    it "works"
    --        $ shouldBe
    --            (parseInput testInput)
    --            empty
