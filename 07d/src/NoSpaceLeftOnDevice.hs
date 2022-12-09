module NoSpaceLeftOnDevice where

import Data.List (foldl')
import qualified Data.List.NonEmpty as NEL
import Data.Map.Strict (Map, empty, null)


type AbsolutePath = NEL.NonEmpty String

type Size = Int

data FSEntity = Dir [AbsolutePath] | File Size
    deriving (Eq, Show)

type DirContent = [FSEntity]

type FS = Map AbsolutePath FSEntity


type ParsingState = (AbsolutePath, DirContent, FS)

--addToFS :: DirContent -> FS -> FS
addToFS directoryContent fs absolutePath = undefined

parse' :: ParsingState -> String -> ParsingState
parse' (absolutePath, directoryContent, fs) ('$':' ':'c':'d':' ':path) =

    let fs' = addToFS directoryContent fs absolutePath

    in case path of
        
        "/" -> (NEL.fromList ["/"], directoryContent, fs)

        ".." -> (NEL.fromList $ NEL.tail absolutePath, directoryContent, fs)
        
        dirName -> (NEL.cons dirName absolutePath, directoryContent, fs)

--parse (absolutePath, directoryContent) ('d':'i':'r':' ':name) = dirName
--parse (absolutePath, directoryContent) fileString = words -- give us [lengthS, name]

parse :: [String] -> ParsingState
parse = let initialParsingState = (NEL.fromList ["/"], [], empty)
    in foldl' parse' initialParsingState


parseInput :: String -> ParsingState --FSEntity
parseInput =
    parse
        -- removing all this lines because they're no use
        . filter (/= "$ ls")
        . lines
