module CampCleanup where

type AssignedSectionsRange = (Int, Int)

type Pair = (AssignedSectionsRange, AssignedSectionsRange)

splitToTupleByChar :: String -> Char -> (String, String)
splitToTupleByChar string char =
    let (leftPart, righPartWithDelimiter) = break (== char) string
        rightPart = drop 1 righPartWithDelimiter
    in (leftPart, rightPart)

parseRange :: String -> AssignedSectionsRange
parseRange string =
    let (rangeStart, rangeEnd) = splitToTupleByChar string '-'
    in (read rangeStart, read rangeEnd)

parsePair :: String -> Pair
parsePair string =
    let (firstElfAssignedSectionsString, secondElfAssignedSectionsString) = splitToTupleByChar string ','
    in (parseRange firstElfAssignedSectionsString, parseRange secondElfAssignedSectionsString)

parseInput :: String -> [Pair]
parseInput = fmap parsePair . lines


