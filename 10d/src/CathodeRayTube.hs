module CathodeRayTube where


import Data.Either (partitionEithers)
import Data.List (stripPrefix)
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

parseInput :: String -> Either [String] [Instruction]
parseInput input =
    let (errors, parsedInstructions) = partitionEithers . fmap parseInstruction $ lines input
    in case length errors of
        0 -> Right parsedInstructions
        _ -> Left errors


