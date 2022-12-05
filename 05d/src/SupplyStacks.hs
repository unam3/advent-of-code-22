module SupplyStacks where

import Data.List (foldl', transpose)
import Data.Map.Strict (Map, fromList)


type StackOfCrates = (Char, String)
type StacksOfCrates = Map Char String

data RearrangementProcedure = RearrangementProcedure {
    cratesQuantitityToMove :: String,
    sourceStack :: Int,
    targetStack :: Int
} deriving (Eq, Show)


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
        . filter ((/= []) . words)
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

takeOnlyNumerical :: [String] -> [String]
takeOnlyNumerical = filter ((\ char -> any (== char) ['0'..'9'])  . head)

parseRearrangementProcedure :: String -> RearrangementProcedure
parseRearrangementProcedure string =
    let [cratesQuantitityToMove', almostSourceStack', almostTargetStack'] = takeOnlyNumerical $ words string
        sourceStack' = read almostSourceStack'
        targetStack' = read almostTargetStack'
    in RearrangementProcedure cratesQuantitityToMove' sourceStack' targetStack'

parseInput :: String -> (StacksOfCrates, [RearrangementProcedure])
parseInput input =
    let [rawStacksOfCrates, rawRearrangementProcedures] = splitByEmptyLines $ lines input
        stacksOfCrates = fromList . fmap parseStackOfCrates $ preprocessStackOfCrates rawStacksOfCrates
    in (stacksOfCrates, [])


-- Crates are moved one at a time, so the first crate to be moved (D) ends up below the second and third crates
