module CalorieCounting where

import Data.List (foldl', sort)

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


getTop3Calories :: [[Int]] -> [Int]
getTop3Calories = take 3 . reverse . sort . fmap sum
