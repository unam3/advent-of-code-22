module RucksackReorganization where

import Data.List (intersect, nub)
import Data.Maybe (fromJust)


type FirstCompartment = String
type SecondCompartment = String

type Rucksack = (FirstCompartment, SecondCompartment)

divideLine :: String -> Rucksack
divideLine line = splitAt (div (length line) 2) line

parseInput :: String -> [Rucksack]
parseInput = fmap divideLine . lines

relations :: [(Char, Int)]
relations = zip (['a'..'z'] ++ ['A'..'Z']) [1..]

findItemFromBothCompartments :: Rucksack -> Char
findItemFromBothCompartments = head . uncurry intersect

getPriority :: Char -> Int
getPriority = fromJust . (`lookup` relations)


type ThreeElfGroup = (String, String, String)

groupTriplets :: [ThreeElfGroup] -> [String] -> [ThreeElfGroup]
groupTriplets acc [] = acc
groupTriplets acc (s1 : s2 : s3 : sn) = groupTriplets ((s1, s2, s3) : acc) sn
groupTriplets _ nonMatchedList = error $ "non matched list: " ++ show nonMatchedList

parseInputP2 :: String -> [ThreeElfGroup]
parseInputP2 = groupTriplets [] . fmap nub . lines

findItemCommonToAllThree :: ThreeElfGroup -> Char
findItemCommonToAllThree (items1, items2, items3) = case intersect items3 $ intersect items1 items2 of
    [item] -> item
    thisCantBe -> error $ show thisCantBe
