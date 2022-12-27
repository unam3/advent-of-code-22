module MonkeyInTheMiddle where


import Data.List (isPrefixOf, foldl', stripPrefix)
import Data.Vector (Vector, (//), fromList)
import Text.Read (readMaybe)

type Round = Int

type InspectedItems = Vector Int

type State = (Round, InspectedItems)


type ItemWorryLevel = Int

data Operation = MultiplyBy Int | Add Int | Sqr
    deriving (Eq, Show)

type DivisibleBy = Int

type ThrowIfTrueTo = Int

type ThrowIfFalseTo = Int

type DivideThenThrow = (DivisibleBy, ThrowIfTrueTo, ThrowIfFalseTo)

type MonkeyState = ([ItemWorryLevel], Operation, DivideThenThrow)

type InputData = Vector MonkeyState

-- from ../05d/src/SupplyStacks
group :: [[String]] -> String -> [[String]]
group acc "" = [] : acc
group (accHead:restAcc) nonEmptyString = (nonEmptyString : accHead) : restAcc
group notMatchedAcc _ = error $ "acc paramater has no match: " ++ show notMatchedAcc

-- from ../05d/src/SupplyStacks
splitByEmptyLines :: [String] -> [[String]]
splitByEmptyLines = reverse . fmap reverse . foldl' group [[]]


parseMonkeySection :: [String] -> ([ItemWorryLevel], Operation, DivideThenThrow)
parseMonkeySection [_, startingItemsS, operationS, divisibleByS, throwIfTrueToS, throwIfFalseToS] =

    let --(Just monkeyNumber) =
        --    stripPrefix "Monkey " monkeyNumberS
        --        -- removing closing ":"
        --        >>= Just . init
        --        >>= readMaybe

        -- Starting items: 62, 68, 56, 65, 94, 78
        (Just startingItemWorryLevels) =
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
    in (startingItemWorryLevels, operation, (divisibleBy, throwIfTrueTo, throwIfFalseTo))

parseMonkeySection nonMatched = error $ "non matched input list: " ++ show nonMatched


parseInput :: String -> InputData
parseInput = fromList . fmap parseMonkeySection . splitByEmptyLines . lines


modify :: ItemWorryLevel -> Operation -> ItemWorryLevel
modify itemWorryLevel Sqr = itemWorryLevel * itemWorryLevel
modify itemWorryLevel (Add n) = n + itemWorryLevel
modify itemWorryLevel (MultiplyBy n) = n * itemWorryLevel


throw :: InputData -> ItemWorryLevel -> Int -> InputData
throw inputData itemWorryLevel monkeyToThrow = undefined 

inspectItem :: MonkeyState -> InputData -> Int -> InputData
inspectItem (itemWorryLevels, operation, (divisibleBy, throwIfTrueTo, throwIfFalseTo)) inputData itemWorryLevel =
    {-
        Monkey inspects an item with a worry level of 79.
        Worry level is multiplied by 19 to 1501.
        Monkey gets bored with item. Worry level is divided by 3 to 500.
        Current worry level is not divisible by 23.
        Item with worry level 500 is thrown to monkey 3.
    -}
    let worryLevel' = modify itemWorryLevel operation
        worryLevelAfterGetBored = round ((fromInteger $ toInteger worryLevel') / 3)
        isDivisibleByTestNumber = (rem worryLevelAfterGetBored divisibleBy) == 0

        -- empty the list for current monky
        -- need monkey index!
        inputDataWithCurrentMunkeyEmptyItemsList = undefined -- (//) inputData [(, [])]

    in if isDivisibleByTestNumber

        -- add item with worryLevelAfterGetBored to monkey from throwIfTrueTo/throwIfFalseTo
        then undefined --- throw to monkey number throwIfTrueTo

        else undefined --- throw to monkey number throwIfFalseTo



inspect :: InputData -> MonkeyState -> InputData
inspect inputData state@(itemWorryLevels, _, _) =
    let pluh = foldl' (inspectItem state) inputData itemWorryLevels
    in undefined
    

