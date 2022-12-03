module RucksackReorganization where

import Data.List (intersect)
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
