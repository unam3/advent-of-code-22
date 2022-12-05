module SupplyStacks where

import Data.List (foldl', transpose)


type StackOfCrates = (Char, String)

data RearrangementProcedure = RearrangementProcedure {
    cratesQuantitityToMove :: Int,
    sourceStack :: Int,
    targetStack :: Int
}


group :: [[String]] -> String -> [[String]]
group acc "" = [] : acc
group (accHead:restAcc) stringWithNumber = (stringWithNumber : accHead) : restAcc
group notMatchedAcc _ = error $ "acc paramater has no match: " ++ show notMatchedAcc

splitByEmptyLines :: [String] -> [[String]]
splitByEmptyLines = reverse . fmap reverse . foldl' group [[]]


preprocessStackOfCrates :: [String] -> [String]
preprocessStackOfCrates =
    -- ["1ZN","2MCD","3P"]
    fmap (takeWhile (/= ' '))
        -- ["1ZN ","2MCD","3P  "]
        . fmap reverse
        -- [" NZ1","DCM2","  P3"]
        . filter ((\ withoutWhitespaces -> if  withoutWhitespaces == [] then False else True ) . words)
        -- ["    "," NZ1","    ","    ","    ","DCM2","    ","    ","    ","  P3","    "]
        . transpose
        . fmap
            (fmap (\char ->
                case char of
                    '[' -> ' '
                    ']' -> ' '
                    _ -> char
            ))


parseStackOfCrates :: String -> StackOfCrates
parseStackOfCrates (stackNumber : crates) = (stackNumber, crates)
parseStackOfCrates nonMatchedInput = error $ "get non matched input: " ++ show nonMatchedInput

--parseInput :: String -> (StackOfCrates, [RearrangementProcedure])
parseInput = fmap id . lines
-- split input into 2 parts: StackOfCratesSource, RearrangementProceduresSource
-- parse them parts


-- Crates are moved one at a time, so the first crate to be moved (D) ends up below the second and third crates
