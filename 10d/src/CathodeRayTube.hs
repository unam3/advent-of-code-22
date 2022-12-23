module CathodeRayTube where


import Data.Bifunctor (second)
import Data.Either (partitionEithers)
import Data.List (elemIndex, foldl', stripPrefix)
import Text.Read (readMaybe)

data Instruction = Noop | Addx Int
    deriving (Eq, Show)

parseInstruction :: String -> Either String Instruction
parseInstruction "noop" = Right Noop
parseInstruction string =
    let valueString = stripPrefix "addx " string
        maybeValue = valueString >>= readMaybe
    in case maybeValue of
    Just value -> Right $ Addx value
    _ -> Left $ "parseInstruction non-matched input: " ++ show string

parseInput :: String -> Either String [Instruction]
parseInput input =
    let (errors, parsedInstructions) = partitionEithers . fmap parseInstruction $ lines input
    in case length errors of
        0 -> Right parsedInstructions
        _ -> Left $ unwords errors

type RegisterXValue = Int
type CycleNumber = Int
type State = (RegisterXValue, CycleNumber)

execute :: State -> Instruction -> State
execute state Noop = second (+1) state
execute (registerValue, cycleNumber) (Addx value) =
    (
        registerValue + value,
        cycleNumber + 2
    )


executeInstructions :: [Instruction] -> Either String State
executeInstructions instructions = Right $ foldl' execute (1, 0) instructions


getSignalStrength :: State -> Int
getSignalStrength (registerValue, cycleNumber) = registerValue * cycleNumber

type SignalStrength = Int
type TargetCycleState = (CycleNumber, Maybe Int)
type ExtendedState = (RegisterXValue, CycleNumber, [TargetCycleState])


updateF :: TargetCycleState -> [TargetCycleState] -> TargetCycleState -> [TargetCycleState]
updateF newTargetCycleState acc currentTargetCycleState =
    let whatToAdd =
            if currentTargetCycleState == (fst newTargetCycleState, Nothing)
            then newTargetCycleState
            else currentTargetCycleState
    in acc ++ [whatToAdd]

update :: [TargetCycleState] -> TargetCycleState -> [TargetCycleState]
update list newTargetCycleState = foldl' (updateF newTargetCycleState) [] list


execute' :: ExtendedState -> Instruction -> ExtendedState
execute' (registerValue, cycleNumber, targetCycleStateList) Noop =
    let cycleNumber' = cycleNumber + 1
        targetCycleState' = update targetCycleStateList (cycleNumber', Just $ cycleNumber' * registerValue)
    in (registerValue, cycleNumber', targetCycleState')

execute' (registerValue, cycleNumber, targetCycleStateList) (Addx value) =
    let cycleNumber1 = cycleNumber + 1
        targetCycleStateList1 = update targetCycleStateList (cycleNumber1, Just $ cycleNumber1 * registerValue)
        cycleNumber2 = cycleNumber1 + 1
        targetCycleStateList2 = update targetCycleStateList1 (cycleNumber2, Just $ cycleNumber2 * registerValue)
    in (registerValue + value, cycleNumber2, targetCycleStateList2)

    
executeInstructions' :: [(CycleNumber, Maybe Int)] -> [Instruction] -> Either String ExtendedState
executeInstructions' targetCycles = Right . foldl' execute' (1, 0, targetCycles)


getSignalStrengthFor :: [Int] -> [Instruction] -> Int
getSignalStrengthFor cycleNumbers instructions = undefined
    
