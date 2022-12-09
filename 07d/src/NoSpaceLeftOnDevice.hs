module NoSpaceLeftOnDevice where

import qualified Data.List as List (foldl')
import qualified Data.List.NonEmpty as NEL
import Data.Map.Strict (Map, foldlWithKey', insert, null, singleton)


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
parse input =
    let parsingState@(absolutePath, directoryContent, fs) = List.foldl' parse' initialParsingState input
        -- if ls-block will be last we must add tempDirectoryContent to fs by hand
        (tempDirectoryContent', fs') = processTempDirectoryContent parsingState
    in (absolutePath, tempDirectoryContent', fs')


parseInput :: String -> ParsingState
parseInput =
    parse
        -- removing all this lines because they're no use
        . filter (/= "$ ls")
        . lines


addSizeIfHasPath :: [String] -> Int -> AbsolutePath -> FSEntity -> Int
addSizeIfHasPath dirAbsolutePath totalSize k fsEntity =
    if NEL.isPrefixOf dirAbsolutePath $ NEL.reverse k
    then case fsEntity of
        Directory -> totalSize
        File size -> totalSize + size
    else totalSize

getDirectorySize :: FS -> AbsolutePath -> Int
getDirectorySize fs dirAbsolutePath = foldlWithKey' (addSizeIfHasPath (reverse $ NEL.toList dirAbsolutePath)) 0 fs

getAllDirectories :: FS -> [AbsolutePath]
getAllDirectories = foldlWithKey' (\ acc k v -> if v == Directory then acc ++ [k] else acc) []

solveFirstPart :: String -> Int
solveFirstPart = 
    (\ (_, _, fs) ->
        sum
            $ filter (<= 100000)
            $ fmap (getDirectorySize fs)
            $ getAllDirectories fs
    ) . parseInput
