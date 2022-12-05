module SupplyStacks where

import Data.List (foldl', transpose)
import Data.Map.Strict (Map, (!), adjust, fromList)
import qualified Data.Map.Strict as M (foldl')


type StackOfCrates = (String, String)
type StacksOfCrates = Map String String

data RearrangementProcedure = RearrangementProcedure {
    cratesQuantitityToMove :: Int,
    sourceStack :: String,
    targetStack :: String
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
    fmap (takeWhile (/= ' ')
        -- ["1ZN ","2MCD","3P  "]
        . reverse
    )
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
parseStackOfCrates (stackNumber : crates) = ([stackNumber], reverse crates)
parseStackOfCrates nonMatchedInput = error $ "get non matched input: " ++ show nonMatchedInput


takeOnlyNumerical :: [String] -> [String]
takeOnlyNumerical = filter ((\ char -> elem char ['0'..'9'])  . head)

parseRearrangementProcedure :: String -> RearrangementProcedure
parseRearrangementProcedure string =
    let [almostCratesQuantitityToMove, sourceStack', targetStack'] = takeOnlyNumerical $ words string
        cratesQuantitityToMove' = read almostCratesQuantitityToMove
    in RearrangementProcedure cratesQuantitityToMove' sourceStack' targetStack'


parseInput :: String -> (StacksOfCrates, [RearrangementProcedure])
parseInput input =
    let [rawStacksOfCrates, rawRearrangementProcedures] = splitByEmptyLines $ lines input
        stacksOfCrates = fromList . fmap parseStackOfCrates $ preprocessStackOfCrates rawStacksOfCrates
        rearrangementProcedures = fmap parseRearrangementProcedure rawRearrangementProcedures
    in (stacksOfCrates, rearrangementProcedures)


rearrange' ::  (String -> String) -> StacksOfCrates -> RearrangementProcedure -> StacksOfCrates
rearrange' deltaCratesOrderingF stacksOfCrates (RearrangementProcedure quantity source target) =
        -- get qtt from source
    let delta = take quantity $ (!) stacksOfCrates source
        -- remove from source first qtty
        stackWithUpdatedSource = adjust (drop quantity) source stacksOfCrates
    -- add to target in reverse order
    in adjust (deltaCratesOrderingF delta ++) target stackWithUpdatedSource

rearrange :: (String -> String) -> StacksOfCrates -> [RearrangementProcedure] -> StacksOfCrates
rearrange deltaCratesOrderingF stacksOfCrates rearrangementProcedures = 
    foldl' (rearrange' deltaCratesOrderingF) stacksOfCrates rearrangementProcedures

getTopCrates :: StacksOfCrates -> String
getTopCrates = M.foldl' (\ acc crate -> acc ++ [head crate]) ""
