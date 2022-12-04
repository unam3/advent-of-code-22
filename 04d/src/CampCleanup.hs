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


isSectionContainAnother :: Pair -> Bool
isSectionContainAnother ((firstElfSectionsRangeStart, firstElfSectionsRangeEnd), (secondElfSectionsRangeStart, secondElfSectionsRangeEnd)) =
    case (
        compare firstElfSectionsRangeStart secondElfSectionsRangeStart,
        compare firstElfSectionsRangeEnd secondElfSectionsRangeEnd
    ) of
        -- .2345678.  2-8
        -- ..34567..  3-7
        (LT, GT) -> True
        -- ..34567..  3-7
        -- .2345678.  2-8
        (GT, LT) -> True
        -- .....6...  6-6
        -- ...456...  4-6
        (GT, EQ) -> True
        -- ...456...  4-6
        -- .....6...  6-6
        (LT, EQ) -> True
        -- ...4.....  4-4
        -- ...456...  4-6
        (EQ, LT) -> True
        -- ...456...  4-6
        -- ...4.....  4-4
        (EQ, GT) -> True
        -- ...4.....  4-4
        -- ...4.....  4-4
        (EQ, EQ) -> True
        _ -> False


doPairOverlap :: Pair -> Bool
doPairOverlap pair@((firstElfSectionsRangeStart, firstElfSectionsRangeEnd), (secondElfSectionsRangeStart, secondElfSectionsRangeEnd)) =
    if isSectionContainAnother pair
        then True
        else case (
                compare firstElfSectionsRangeStart secondElfSectionsRangeStart,
                compare firstElfSectionsRangeEnd secondElfSectionsRangeEnd,
                compare firstElfSectionsRangeEnd secondElfSectionsRangeStart,
                compare firstElfSectionsRangeStart secondElfSectionsRangeEnd
            ) of
                -- ....567..  5-7
                -- ......789  7-9
                (LT, LT, EQ, _) -> True

                -- ....5678.  5-8
                -- ......789  7-9
                (LT, LT, GT, _) -> True

                -- ......789  7-9
                -- ....567..  5-7
                (GT, GT, _, EQ) -> True

                -- ......789  7-9
                -- ....5678.  5-8
                (GT, GT, _, LT) -> True

                _ -> False
