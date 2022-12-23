module CathodeRayTube where


import Data.Bifunctor (second)
import Data.Either (partitionEithers)
import Data.List (foldl', stripPrefix)
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


--executeInstructions :: Either [String] [Instruction] -> Either String State
--executeInstructions (Left errors) = Left $ show errors
--executeInstructions (Right instructions) = Right $ foldl' execute (1, 0) instructions
executeInstructions :: [Instruction] -> Either String State
executeInstructions instructions = Right $ foldl' execute (1, 0) instructions
