module CalorieCounting where

import Data.List (foldl')

groupCaloriesF :: [[String]] -> String -> [[String]]
groupCaloriesF acc "" = [] : acc
groupCaloriesF (accHead:restAcc) stringWithNumber = (stringWithNumber : accHead) : restAcc
groupCaloriesF notMatchedAcc _ = error $ "acc paramater has no match: " ++ show notMatchedAcc

parseNumberFromString :: String -> Int
parseNumberFromString = read

groupCalories :: [String] -> [[String]]
groupCalories = reverse . fmap reverse . foldl' groupCaloriesF [[]]

parseInput :: String -> [[Int]]
parseInput =  fmap (fmap parseNumberFromString) . groupCalories . lines


findMostCalories :: [[Int]] -> Int
findMostCalories = maximum . fmap sum


solveTest :: IO ()
solveTest = readFile "testInput"
    >>= print
        . parseInput

solve :: IO ()
solve = readFile "input.txt"
    >>= print
        . parseInput

solveTest2 :: IO ()
solveTest2 = readFile "testInput"
    >>= print
        . parseInput

solve2 :: IO ()
solve2 = readFile "input.txt"
    >>= print
        . parseInput

