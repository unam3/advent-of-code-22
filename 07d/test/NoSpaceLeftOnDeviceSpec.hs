module NoSpaceLeftOnDeviceSpec where 

import qualified Data.List.NonEmpty as NEL
import Data.Map.Strict (fromList, singleton)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import NoSpaceLeftOnDevice

spec :: Spec
spec = do
    testInput <- runIO $ readFile "testInput"

    --input <- runIO $ readFile "input.txt"

    describe "processTempDirectoryContent" $ do
        it "works"
            $ shouldBe
                (processTempDirectoryContent
                    (
                        (NEL.fromList ["/"]),
                        [
                            ("a", File 123),
                            ("pluh", Directory)
                        ],
                        (singleton (NEL.fromList ["/"]) Directory)
                    )
                )
                (
                    [],
                    fromList [
                        (NEL.fromList ["/"], Directory),
                        (NEL.fromList ["a", "/"], File 123),
                        (NEL.fromList ["pluh", "/"], Directory)
                    ]
                )
                

    describe "parse" $ do
        it "works for consecutive \"cd /\""
            $ shouldBe
                (parse ["$ cd /", "$ cd /"])
                initialParsingState

        it "works for directory names: \"cd pluh\""
            $ shouldBe
                ((\(absolutePath, _, _) -> absolutePath) $ parse ["$ cd pluh", "$ cd meh"])
                (NEL.fromList ["meh", "pluh", "/"])

        it "works for going up: \"cd ..\""
            $ shouldBe
                ((\(absolutePath, _, _) -> absolutePath) $ parse ["$ cd pluh", "$ cd meh", "$ cd ..", "$ cd .."])
                (NEL.fromList ["/"])
                
        it "works for consecutive ls-output directories lines (dir pluh)"
            $ shouldBe
                ((\(_, directoryContent, _) -> directoryContent) $ parse ["dir a", "dir b"])
                [("a", Directory), ("b", Directory)]
                
        it "works for consecutive ls-output if files lines (123 pluh)"
            $ shouldBe
                ((\(_, directoryContent, _) -> directoryContent) $ parse ["123 a", "321 b"])
                [("a", File 123), ("b", File 321)]
                
        it "updates fs after ls-ouput block"
            $ shouldBe
                (parse ["123 a", "dir pluh", "$ cd pluh"])
                (
                    NEL.fromList ["pluh", "/"],
                    [],
                    fromList [
                        (NEL.fromList ["/"], Directory),
                        (NEL.fromList ["a", "/"], File 123),
                        (NEL.fromList ["pluh", "/"], Directory)
                    ]
                )
                

    --describe "f" $ do
    --    it "works"
    --        $ shouldBe
    --            42
    --            42
