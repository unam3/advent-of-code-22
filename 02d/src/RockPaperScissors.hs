module RockPaperScissors where

-- opponents figure
-- A — rock
-- B — paper
-- C — scissors
data FirstColumn = A | B | C
    deriving (Show, Eq)

-- I should play in response
-- X — rock 1
-- Y — paper 2
-- Z — scissors 3
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


-- The score for a single round is the score for the shape you selected (1 for Rock, 2 for Paper, and 3 for Scissors) plus the score for the outcome of the round (0 if you lost, 3 if the round was a draw, and 6 if you won).

playRound :: Round -> Int
-- If both players choose the same shape, the round instead ends in a draw.
playRound (A, X) = 1 + 3
playRound (B, Y) = 2 + 3
playRound (C, Z) = 3 + 3

-- opponent wins
-- Rock defeats Scissors
playRound (A, Z) = 0 + 3
-- Scissors defeats Paper
playRound (C, Y) = 0 + 2
-- Paper defeats Rock
playRound (B, X) = 0 + 1

-- I win
-- Rock defeats Scissors
playRound (C, X) = 6 + 1
-- Scissors defeats Paper
playRound (B, Z) = 6 + 3
-- Paper defeats Rock
playRound (A, Y) = 6 + 2


getTotalScore :: [Int] -> Int
getTotalScore = sum


-- P2.
-- Second column says how the round needs to end:
--  X means you need to lose,
--  Y means you need to end the round in a draw,
--  Z means you need to win.

playRound' :: Round -> Int
-- If both players choose the same shape, the round instead ends in a draw.
playRound' (A, Y) = 1 + 3
playRound' (B, Y) = 2 + 3
playRound' (C, Y) = 3 + 3

-- opponent wins
-- Rock defeats Scissors
playRound' (A, X) = 0 + 3
-- Scissors defeats Paper
playRound' (C, X) = 0 + 2
-- Paper defeats Rock
playRound' (B, X) = 0 + 1

-- I win
-- Rock defeats Scissors
playRound' (C, Z) = 6 + 1
-- Scissors defeats Paper
playRound' (B, Z) = 6 + 3
-- Paper defeats Rock
playRound' (A, Z) = 6 + 2
