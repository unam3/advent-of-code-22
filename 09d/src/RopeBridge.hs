module RopeBridge where

data Motion = R Int | U Int | L Int | D Int
    deriving (Eq, Show)

parseInput :: String -> [Motion] 
parseInput = undefined


type Coords = (Int, Int)
type HeadTailCoords = (Coords, Coords)
type TailVisitedAtLeastOnce = [Coords]
type State = (HeadTailCoords, TailVisitedAtLeastOnce)

modelMotion :: Motion -> State -> State
modelMotion = undefined
