module NoSpaceLeftOnDeviceSpec where 

import qualified Data.List.NonEmpty as NEL
import Data.List.NonEmpty ( NonEmpty( (:|) ) )
import Data.Map.Strict (fromList, singleton)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import NoSpaceLeftOnDevice

spec :: Spec
spec = do
    testInput <- runIO $ readFile "testInput"

    input <- runIO $ readFile "input.txt"

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
                (parse ["dir a", "dir b"])
                (
                    NEL.fromList ["/"],
                    [],
                    fromList [
                        (NEL.fromList ["/"], Directory),
                        (NEL.fromList ["a", "/"], Directory),
                        (NEL.fromList ["b", "/"], Directory)
                    ]
                )
                
        it "works for consecutive ls-output if files lines (123 pluh)"
            $ shouldBe
                (parse ["123 a", "321 b"])
                (
                    NEL.fromList ["/"],
                    [],
                    fromList [
                        (NEL.fromList ["/"], Directory),
                        (NEL.fromList ["a", "/"], File 123),
                        (NEL.fromList ["b", "/"], File 321)
                    ]
                )
                
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
                

    describe "parseInput" $ do
        it "works for testInput (checked by hand)"
            $ shouldBe
                (parseInput testInput)
                ("d" :| ["/"],[],fromList [("/" :| [],Directory),("a" :| ["/"],Directory),("b.txt" :| ["/"],File 14848514),("c.dat" :| ["/"],File 8504156),("d" :| ["/"],Directory),("d.ext" :| ["d","/"],File 5626152),("d.log" :| ["d","/"],File 8033020),("e" :| ["a","/"],Directory),("f" :| ["a","/"],File 29116),("g" :| ["a","/"],File 2557),("h.lst" :| ["a","/"],File 62596),("i" :| ["e","a","/"],File 584),("j" :| ["d","/"],File 4060174),("k" :| ["d","/"],File 7214296)])

    describe "getDirectorySize" $ do
        it "works for testInput \"/\""
            $ shouldBe
                ((\ (_, _, fs) -> getDirectorySize fs (NEL.fromList ["/"])) $ parseInput testInput)
                48381165
                
    describe "getAllDirectories" $ do
        it "works for testInput"
            $ shouldBe
                ((\ (_, _, fs) -> getAllDirectories fs) $ parseInput testInput)
                ["/" :| [],"a" :| ["/"],"d" :| ["/"],"e" :| ["a","/"]]

    describe "solveFirstPart" $ do
        it "works for testInput"
            $ shouldBe
                (solveFirstPart testInput)
                95437
                
        it "works for input.txt"
            $ shouldBe
                (solveFirstPart input)
                1077191
