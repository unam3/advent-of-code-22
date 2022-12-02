module RockPaperScissors where

-- opponents figure
-- A — rock
-- B — paper
-- C — scissors
data FirstColumn = A | B | C
    deriving (Show, Eq)

-- I should play in response
-- X — rock
-- Y — paper
-- Z — scissors
data SecondColumn = X | Y | Z
    deriving (Show, Eq)

type Round = (FirstColumn, SecondColumn)

parseFirstColumn :: Char -> FirstColumn
parseFirstColumn 'A' = A
parseFirstColumn 'B' = B
parseFirstColumn 'C' = C
parseFirstColumn wrongInput = error $ "wrong input: " ++ show wrongInput

parseSecondColumn :: Char -> SecondColumn
parseSecondColumn 'X' = X
parseSecondColumn 'Y' = Y
parseSecondColumn 'Z' = Z
parseSecondColumn wrongInput = error $ "wrong input: " ++ show wrongInput

parseRound :: String -> Round
parseRound (firstColumn : ' ' : secondColumn : []) = (parseFirstColumn firstColumn, parseSecondColumn secondColumn)
parseRound wrongInput = error $ "wrong input: " ++ wrongInput

parseInput :: String -> [Round]
parseInput = fmap parseRound . lines


playRound :: Round -> Int
playRound = undefined

