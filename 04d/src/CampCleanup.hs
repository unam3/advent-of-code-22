module CampCleanup where

type AssignedSectionsRange = (Int, Int)

type Pair = (AssignedSectionsRange, AssignedSectionsRange)

parseRange :: String -> AssignedSectionsRange
parseRange string =
    let (rangeStart, almostRangeEnd) = break (== '-') string
        rangeEnd = drop 1 almostRangeEnd
    in (read rangeStart, read rangeEnd)

--parsePair :: String -> Pair
--parsePair = parseRange . splitLineByComa

parseInput :: String -> [Pair]
parseInput = undefined
    -- fmap id . lines


