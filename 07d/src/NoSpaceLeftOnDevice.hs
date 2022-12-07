module NoSpaceLeftOnDevice where


type Name = String

type Size = Int

--data FSEntity = Dir Name DirContent | File Name Size

--data FSEntity1 = Dir DirContent | File Int | NotYetFilled
-- type AbsolutePath = [String]
-- type FS = Map AbsolutePath FSEntity1

data FSEntity = Dir Name | File Name Size

type DirContent = [FSEntities]

type ParsingState = (AbsolutePath, DirContent)

-- currently parsing block:
--  cd
--  ls output

--parse :: ParsingState -> String -> ParsingState
parse (absolutePath, directoryContent) ('$':' ':'c':'d':' ':path) = 
parse (absolutePath, directoryContent) ('d':'i':'r':' ':name) = dirName
parse (absolutePath, directoryContent) fileString = words -- give us [lengthS, name]

--parseInput :: String -> FSEntity
parseInput = parse . filter (/= "$ ls"). lines


