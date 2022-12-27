module CathodeRayTube where


import Data.Bifunctor (second)
import Data.Either (partitionEithers)
import Data.List (foldl', stripPrefix)
import Data.Maybe (catMaybes)
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
getSignalStrengthFor cycleNumbers =
    (\ (Right (_, _, targetCycleStateList)) -> sum . catMaybes $ fmap snd targetCycleStateList)
        . executeInstructions' ((zip cycleNumbers $ cycle [Nothing]))


type IsPixelLit = Bool
type CRTState = [IsPixelLit]
type PartIIState = (RegisterXValue, CycleNumber, CRTState)


haveToLightPixel :: RegisterXValue -> CycleNumber -> Bool
haveToLightPixel spriteMiddle pixelCurrentlyBeingDrawnNumber =
    let spriteMiddleInCycles = spriteMiddle + 1
    in pixelCurrentlyBeingDrawnNumber == (subtract 1 spriteMiddleInCycles)
        || pixelCurrentlyBeingDrawnNumber == spriteMiddleInCycles
        || pixelCurrentlyBeingDrawnNumber == 1 + spriteMiddleInCycles

adjustCycleNumber :: CycleNumber -> CycleNumber
adjustCycleNumber n
    | n <= 40 = n
    | otherwise =
        let nMod40 = mod n 40
        in if nMod40 == 0
            then 40
            else nMod40

executeII :: CycleNumber -> PartIIState -> Instruction -> PartIIState
executeII targetCycleNumber state@(registerValue, cycleNumber, crtState) Noop =

    if cycleNumber >= targetCycleNumber
    
        then state
        
        else 
            let cycleNumber' = cycleNumber + 1
                isPixelLit = haveToLightPixel registerValue cycleNumber'
                crtState' = crtState ++ [isPixelLit]
            in (registerValue, cycleNumber', crtState')

executeII targetCycleNumber state@(registerValue, cycleNumber, crtState) (Addx value) =
    let cycleNumber1 = cycleNumber + 1
        isPixelLit1 = haveToLightPixel registerValue cycleNumber1
        crtState1 = crtState ++ [isPixelLit1]

        cycleNumber2 = cycleNumber1 + 1
        isPixelLit2 = haveToLightPixel registerValue cycleNumber2
        crtState2 = crtState1 ++ [isPixelLit2]

    in if cycleNumber >= targetCycleNumber
    
        then state

        else if cycleNumber1 >= targetCycleNumber

            then (registerValue, cycleNumber1, crtState1)

            else (registerValue + value, cycleNumber2, crtState2)


executeInstructionsII :: Int -> [Instruction] -> Either String PartIIState
executeInstructionsII targetCycleNumber = Right . foldl' (executeII targetCycleNumber) (1, 0, [])

boolToPixel :: Bool -> Char
boolToPixel True = '#'
boolToPixel False = '.'

pixelToBool :: Char -> Bool
pixelToBool '#' = True
pixelToBool _ = False

fmapBoolToPixel :: PartIIState -> (RegisterXValue, CycleNumber, String)
fmapBoolToPixel (registerValue, cycleNumber, crtState) = (registerValue, cycleNumber, fmap boolToPixel crtState)


splitCRTStateInSix :: CRTState -> [CRTState]
splitCRTStateInSix crtState = --foldl' (\ acc _ = splitAt 40) 1 [1..6]
    let (forty, rest200) = splitAt 40 crtState
        (secondForty, rest160) = splitAt 40 rest200
        (thirdForty, rest120) = splitAt 40 rest160
        (fourthForty, rest80) = splitAt 40 rest120
        (fifthForty, rest40) = splitAt 40 rest80
    in [forty, secondForty, thirdForty, fourthForty, fifthForty, rest40]


assignCycleNumber :: (CycleNumber, [(CycleNumber, Instruction)])
    -> Instruction
    -> (CycleNumber, [(CycleNumber, Instruction)])
assignCycleNumber (cycleCounter, accList) instruction =

    let cycleCounter' = cycleCounter + 1
        cycleCounter'' = cycleCounter + 2
        isNoop = instruction == Noop
        newCycleCounter =
            if isNoop
            then cycleCounter'
            else cycleCounter''
        listToAdd =
            if isNoop
            then [(cycleCounter', instruction)]
            else [(cycleCounter', instruction), (cycleCounter'', instruction)]


    in (newCycleCounter, accList ++ listToAdd)

mapCycleNumbers :: [Instruction] -> [(CycleNumber, Instruction)]
mapCycleNumbers =
    snd
        . foldl'
            assignCycleNumber
            (0, [])


collectCRTStateAfter240Cycles :: [Instruction] -> String
collectCRTStateAfter240Cycles = undefined
