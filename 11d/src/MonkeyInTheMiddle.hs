module MonkeyInTheMiddle where


import Data.Function (fix)
import Data.List (isPrefixOf, foldl', sortBy, stripPrefix)
import qualified Data.List as L (length)
import Data.Vector (Vector, (!), (//), fromList, ifoldl', length)
import qualified Data.Vector as V (foldl')
import Prelude hiding (length, round)
import Text.Read (readMaybe)

type Round = Int

type InspectedItems = Vector Int

type State = (Round, InspectedItems)


type ItemWorryLevel = Int

type NumberOfInspectedItems = Int

data Operation = MultiplyBy Int | Add Int | Sqr
    deriving (Eq, Show)

type DivisibleBy = Int

type ThrowIfTrueTo = Int

type ThrowIfFalseTo = Int

type DivideThenThrow = (DivisibleBy, ThrowIfTrueTo, ThrowIfFalseTo)

type MonkeyState = (Operation, DivideThenThrow, NumberOfInspectedItems, [ItemWorryLevel])

type InputData = Vector MonkeyState

-- from ../05d/src/SupplyStacks
group :: [[String]] -> String -> [[String]]
group acc "" = [] : acc
group (accHead:restAcc) nonEmptyString = (nonEmptyString : accHead) : restAcc
group notMatchedAcc _ = error $ "acc paramater has no match: " ++ show notMatchedAcc

-- from ../05d/src/SupplyStacks
splitByEmptyLines :: [String] -> [[String]]
splitByEmptyLines = reverse . fmap reverse . foldl' group [[]]


parseMonkeySection :: [String] -> MonkeyState
parseMonkeySection [_, startingItemsS, operationS, divisibleByS, throwIfTrueToS, throwIfFalseToS] =

    let --(Just monkeyNumber) =
        --    stripPrefix "Monkey " monkeyNumberS
        --        -- removing closing ":"
        --        >>= Just . init
        --        >>= readMaybe

        initialNumberOfInspectedItems = 0

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
    in (
        operation,
        (divisibleBy, throwIfTrueTo, throwIfFalseTo),
        initialNumberOfInspectedItems,
        startingItemWorryLevels
    )

parseMonkeySection nonMatched = error $ "non matched input list: " ++ show nonMatched


parseInput :: String -> InputData
parseInput = fromList . fmap parseMonkeySection . splitByEmptyLines . lines


modify :: ItemWorryLevel -> Operation -> ItemWorryLevel
modify itemWorryLevel Sqr = itemWorryLevel * itemWorryLevel
modify itemWorryLevel (Add n) = n + itemWorryLevel
modify itemWorryLevel (MultiplyBy n) = n * itemWorryLevel


type WorryLevelModifier = (Int -> Int)

inspectItem :: WorryLevelModifier -> Int -> MonkeyState -> InputData -> Int -> InputData
inspectItem
    worryLevelModifier
    monkeyIndex
    (operation, (divisibleBy, throwIfTrueTo, throwIfFalseTo), numberOfInspectedItems, _)
    inputData
    itemWorryLevel =
    {-
        Monkey inspects an item with a worry level of 79.
        Worry level is multiplied by 19 to 1501.
        Monkey gets bored with item. Worry level is divided by 3 to 500.
        Current worry level is not divisible by 23.
        Item with worry level 500 is thrown to monkey 3.
    -}
    let worryLevel' = modify itemWorryLevel operation
        worryLevelAfterGetBored = worryLevelModifier worryLevel'
        isDivisibleByTestNumber = (rem worryLevelAfterGetBored divisibleBy) == 0
        monkeyToThrow =
            if isDivisibleByTestNumber
            then throwIfTrueTo
            else throwIfFalseTo
         -- monkeyToThrowState
        (operation', divideThenThrow', numberOfInspectedItems, itemWorryLevels') = (!) inputData monkeyToThrow
        newItemWorryLevels' = itemWorryLevels' ++ [worryLevelAfterGetBored]

    in if monkeyIndex == monkeyToThrow
        then error "Thow to itself encountered. Need to add support for item list modification."
        else (//)
            inputData
            [(monkeyToThrow, (operation', divideThenThrow', numberOfInspectedItems, newItemWorryLevels'))]


inspect :: WorryLevelModifier -> InputData -> Int -> MonkeyState -> InputData
inspect
    worryLevelModifier
    inputData
    monkeyIndex
    state@(operation, divideThenThrow, numberOfInspectedItems, itemWorryLevels) =

    let newInputData = foldl' (inspectItem worryLevelModifier monkeyIndex state) inputData itemWorryLevels
        newNumberOfInspectedItems = numberOfInspectedItems + L.length itemWorryLevels
        newInputDataWithoutInspectedItems =
            (//)
                newInputData
                [(
                    monkeyIndex,
                    (operation, divideThenThrow, newNumberOfInspectedItems, [])
                )]
    in newInputDataWithoutInspectedItems

inspectWrapper :: WorryLevelModifier -> InputData -> Int -> InputData
inspectWrapper worryLevelModifier inputData monkeyIndex =
    inspect
        worryLevelModifier
        inputData
        monkeyIndex
        $ (!) inputData monkeyIndex

round :: WorryLevelModifier -> InputData -> InputData
round worryLevelModifier inputData = foldl' (inspectWrapper worryLevelModifier) inputData [0 .. subtract 1 $ length inputData]

--mutatingAccIfoldl :: (Vector a -> Int -> a -> Vector a) -> Vector a -> Vector a
--mutatingAccIfoldl f acc =
--
--    let fWrapper acc index = f
--            acc
--            index
--            $ (!) acc index
--    
--    in foldl' fWrapper acc [0 .. subtract 1 $ length acc]
--
--round :: InputData -> InputData
--round inputData = mutatingAccIfoldl inspect inputData

runNRounds :: WorryLevelModifier -> Int -> InputData -> InputData
runNRounds worryLevelModifier n inputData = foldl' (\ accV _ -> round worryLevelModifier accV) inputData [0..n]

getMonkeyBusinessLevel :: InputData -> Int
getMonkeyBusinessLevel =
    product
        . take 2
        . sortBy (flip compare)
        . V.foldl'
            (\ acc (_, _, numberOfInspectedItems, _) -> acc ++ [numberOfInspectedItems])
            []


dropWorryLevel :: Int -> Int
dropWorryLevel worryLevel = fromInteger $ floor ((fromInteger $ toInteger worryLevel) / 3)
