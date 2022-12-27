module MonkeyInTheMiddle where


import Data.List (isPrefixOf, foldl', stripPrefix)
import Data.Vector (Vector, fromList)
import Text.Read (readMaybe)

type Round = Int

type InspectedItems = Vector Int

type State = (Round, InspectedItems)


type StartingItems = [Int]

data Operation = MultiplyBy Int | Add Int | Sqr
    deriving (Eq, Show)

type DivisibleBy = Int

type ThrowIfTrueTo = Int

type ThrowIfFalseTo = Int

type DivideThenThrow = (DivisibleBy, ThrowIfTrueTo, ThrowIfFalseTo)

type InputData = Vector (StartingItems, Operation, DivideThenThrow)

-- from ../05d/src/SupplyStacks
group :: [[String]] -> String -> [[String]]
group acc "" = [] : acc
group (accHead:restAcc) nonEmptyString = (nonEmptyString : accHead) : restAcc
group notMatchedAcc _ = error $ "acc paramater has no match: " ++ show notMatchedAcc

-- from ../05d/src/SupplyStacks
splitByEmptyLines :: [String] -> [[String]]
splitByEmptyLines = reverse . fmap reverse . foldl' group [[]]


parseMonkeySection :: [String] -> (StartingItems, Operation, DivideThenThrow)
parseMonkeySection [_, startingItemsS, operationS, divisibleByS, throwIfTrueToS, throwIfFalseToS] =

    let --(Just monkeyNumber) =
        --    stripPrefix "Monkey " monkeyNumberS
        --        -- removing closing ":"
        --        >>= Just . init
        --        >>= readMaybe

        -- Starting items: 62, 68, 56, 65, 94, 78
        (Just startingItems) =
            stripPrefix "  Starting items: " startingItemsS
                >>= Just . (\ numbers -> "[" ++ numbers ++ "]")
                >>= readMaybe

        (Just operation) =
            stripPrefix "  Operation: new = old " operationS
                >>= Just
                    . (\ op ->
                        if op == "* old"
                            then Sqr
                            else if isPrefixOf "+ " op
                                then Add . read $ drop 2 op
                                else MultiplyBy . read $ drop 2 op
                    )

        (Just divisibleBy) =
            stripPrefix "  Test: divisible by " divisibleByS
                >>= readMaybe

        (Just throwIfTrueTo) =
            stripPrefix "    If true: throw to monkey " throwIfTrueToS
                >>= readMaybe

        (Just throwIfFalseTo) =
            stripPrefix "    If false: throw to monkey " throwIfFalseToS
                >>= readMaybe

    --in (monkeyNumber, startingItems, operation, (divisibleBy, throwIfTrueTo, throwIfFalseTo))
    in (startingItems, operation, (divisibleBy, throwIfTrueTo, throwIfFalseTo))

parseMonkeySection nonMatched = error $ "non matched input list: " ++ show nonMatched


parseInput :: String -> InputData
parseInput = fromList . fmap parseMonkeySection . splitByEmptyLines . lines


