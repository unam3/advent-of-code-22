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

--update :: [TargetCycleState] -> TargetCycleState -> Int -> [TargetCycleState]
--update list newTargetCycleState = foldl' (updateF newTargetCycleState) [] list


--f :: CycleNumber -> RegisterXValue -> [TargetCycleState] -> TargetCycleState -> [TargetCycleState]
--f cycleNumber rValue acc currentTuple =
--    if currentTuple == (cycleNumber, Nothing)
--    then acc ++ [(cycleNumber, Just rValue)]
--    else currentTuple

--foldl' (f currentCycleNumber registerValue) [] list

execute' :: ExtendedState -> Instruction -> ExtendedState
execute' (registerValue, cycleNumber, targetCycleState) Noop =
    let cycleNumber' = cycleNumber + 1
        -- update targetCycleState element if cycleNumber is in it
        targetCycleState' = undefined -- foldl' () [] targetCycleState
    in (registerValue, cycleNumber', targetCycleState')

execute' (registerValue, cycleNumber, targetCycleState) (Addx value) =
    undefined --(registerValue, cycleNumber, targetCycleState)

executeInstructions' :: [Instruction] -> [(CycleNumber, Maybe Int)] -> Either String ExtendedState
executeInstructions' instructions targetCycles = undefined -- Right $ foldl' execute' (1, 0, targetCycles) instructions



getSignalStrengthFor :: [Int] -> [Instruction] -> Int
getSignalStrengthFor cycleNumbers instructions = undefined
    
