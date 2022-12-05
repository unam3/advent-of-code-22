module SupplyStacks where

import Data.List (foldl')


type StackOfCrates = [Char]

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


--parseInput :: String -> (StackOfCrates, [RearrangementProcedure])
parseInput = fmap id . lines
-- split input into 2 parts: StackOfCratesSource, RearrangementProceduresSource
-- parse them parts


-- Crates are moved one at a time, so the first crate to be moved (D) ends up below the second and third crates
