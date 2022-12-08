module NoSpaceLeftOnDevice where

import qualified Data.List.NonEmpty as NEL

type AbsolutePath = [String]

type Size = Int

data FSEntity = Dir [AbsolutePath] | File Size

type DirContent = [FSEntity]

type FS = Map AbsolutePath FSEntity


type ParsingState = (AbsolutePath, DirContent, FS)

-- currently parsing block:
--  cd
--  ls output

--parse :: ParsingState -> String -> ParsingState
parse (absolutePath, directoryContent, ) ('$':' ':'c':'d':' ':path) =
    case path of
        "/" -> (["/"], directoryContent)
        ".." -> (NEL.tail absolutePath, directoryContent)
        dirName ->  (NEL.:|) dirName absolutePath , directoryContent)
parse (absolutePath, directoryContent) ('d':'i':'r':' ':name) = dirName
parse (absolutePath, directoryContent) fileString = words -- give us [lengthS, name]

--parseInput :: String -> FSEntity
parseInput = parse . filter (/= "$ ls"). lines


