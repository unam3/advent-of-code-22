module NoSpaceLeftOnDeviceSpec where 

import qualified Data.List.NonEmpty as NEL
import Data.Map.Strict (empty)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import NoSpaceLeftOnDevice

spec :: Spec
spec = do
    testInput <- runIO $ readFile "testInput"

    --input <- runIO $ readFile "input.txt"

    let initialParsingState = (NEL.fromList ["/"], [], empty)

    describe "parse" $ do
        it "works for consecutive \"cd /\""
            $ shouldBe
                (parse ["$ cd /", "$ cd /"])
                initialParsingState

    --describe "f" $ do
    --    it "works"
    --        $ shouldBe
    --            42
    --            42
