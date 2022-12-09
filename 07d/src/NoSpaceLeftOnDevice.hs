module NoSpaceLeftOnDevice where

import qualified Data.List as List (foldl')
import qualified Data.List.NonEmpty as NEL
import Data.Map.Strict (Map, singleton, insert, null)


type AbsolutePath = NEL.NonEmpty String

type Name = String

type Size = Int

-- to represent filesystem without map
--data FSEntity = DirectoryContent Name [FSEntity] | File Size
--    deriving (Eq, Show)

-- to represent filesystem with map
data FSEntity = Directory | File Size
    deriving (Eq, Show)

type FS = Map AbsolutePath FSEntity

type TempDirContent = [(Name, FSEntity)]

type ParsingState = (AbsolutePath, TempDirContent, FS)


insertFSEntity :: AbsolutePath -> FS -> (Name, FSEntity) -> FS
insertFSEntity absolutePath fs (name, entity) = insert (NEL.cons name absolutePath) entity fs

processTempDirectoryContent :: ParsingState -> (TempDirContent, FS)
processTempDirectoryContent parsingState@(absolutePath, tempDirectoryContent, fs) =
    if Prelude.null tempDirectoryContent
    then (tempDirectoryContent, fs)
    else let updatedFS = List.foldl' (insertFSEntity absolutePath) fs tempDirectoryContent
    in ([], updatedFS)


parse' :: ParsingState -> String -> ParsingState
parse' parsingState@(absolutePath, directoryContent, fs) ('$':' ':'c':'d':' ':path) =

    -- block of "ls" output lines must be assigned to previous absolutPath directory content
    let (tempDirectoryContent, fs') = processTempDirectoryContent parsingState

    in case path of
        
        "/" -> (NEL.fromList ["/"], tempDirectoryContent, fs')

        ".." -> (NEL.fromList $ NEL.tail absolutePath, tempDirectoryContent, fs')
        
        dirName -> (NEL.cons dirName absolutePath, tempDirectoryContent, fs')

parse' (absolutePath, directoryContent, fs) ('d':'i':'r':' ':name) =
    (
        absolutePath,
        directoryContent ++ [(name, Directory)],
        fs
    )

parse' (absolutePath, directoryContent, fs) fileString =
    let [fileSize, name] = words fileString
    in (
        absolutePath,
        directoryContent ++ [(name, File $ read fileSize)],
        fs
    )

initialParsingState :: ParsingState
initialParsingState = (NEL.fromList ["/"], [], singleton (NEL.fromList ["/"]) Directory)

parse :: [String] -> ParsingState
parse = List.foldl' parse' initialParsingState


parseInput :: String -> ParsingState --FSEntity
parseInput =
    parse
        -- removing all this lines because they're no use
        . filter (/= "$ ls")
        . lines
