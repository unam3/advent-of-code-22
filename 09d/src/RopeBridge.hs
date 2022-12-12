module RopeBridge where


import Data.List (foldl', union)

data Motion = R Int | U Int | L Int | D Int
    deriving (Eq, Show)

parseMotion :: String -> Motion
parseMotion (direction : ' ' : number) =
    case direction of
        'R' -> R $ read number
        'U' -> U $ read number
        'L' -> L $ read number
        'D' -> D $ read number
        undefinedDirection -> error $ "wrong motion direction: " ++ show undefinedDirection

parseMotion wrongInput = error $ "wrong input for parseMotion: " ++ wrongInput


parseInput :: String -> [Motion] 
parseInput = fmap parseMotion . lines


type Coords = (Int, Int)
type HeadTailCoords = (Coords, Coords)
type TailVisitedAtLeastOnce = [Coords]
type State = (HeadTailCoords, TailVisitedAtLeastOnce)

animate :: Motion -> State -> Int -> State
animate (U _) state@(((hx, hy), (tx, ty)), tailVisitedAtLeastOnce) _ =
    -- consider 9 cases tail to head relations:
    --    - touching:
    --        - to left, to right, to top, to bottom
    --        - diagonally adjacent:
    --            - diagonal NE, NW, SW, SE
    -- consult the `animate file`
    -- overlapping
    if hx == tx && hy == ty
    then (((hx, hy + 1), (tx, ty)), tailVisitedAtLeastOnce)
    -- touching: head to the right from tail
    else if hy == ty && hx < tx
    then (((hx, hy + 1), (tx, ty)), tailVisitedAtLeastOnce)
    -- touching: head to the left from tail
    else if hy == ty && hx > tx
    then (((hx, hy + 1), (tx, ty)), tailVisitedAtLeastOnce)
    -- touching: head is below the tail
    else if hy < ty && hx == tx
    then (((hx, hy + 1), (tx, ty)), tailVisitedAtLeastOnce)
    -- touching: head is above the tail
    else if hy > ty && hx == tx
    then let newTailCoords = (tx, ty + 1)
        in (((hx, hy + 1), newTailCoords), union tailVisitedAtLeastOnce [newTailCoords])
    -- touching (diagonally adjacent) head is to NE from tail
    else if hy > ty && hx < tx
    then let newTailCoords = (tx + 1, ty + 1)
        in (((hx, hy + 1), newTailCoords), union tailVisitedAtLeastOnce [newTailCoords])
    -- touching (diagonally adjacent) head is to NW from tail
    else if hy > ty && hx < tx
    then let newTailCoords = (tx - 1, ty + 1)
        in (((hx, hy + 1), newTailCoords), union tailVisitedAtLeastOnce [newTailCoords])
    -- touching (diagonally adjacent) head is to SW from tail
    else if hy > ty && hx < tx
    then (((hx, hy + 1), (tx, ty)), tailVisitedAtLeastOnce)
    -- touching (diagonally adjacent) head is to SE from tail
    else if hy > ty && hx > tx
    then (((hx, hy + 1), (tx, ty)), tailVisitedAtLeastOnce)
    else error $ "animate U: unexpected head/tail configuration: " ++ show state

    
animate _ _ _ = undefined

modelMotion' :: State -> Motion -> State
modelMotion' state motion@(U number) =
    -- for debug
    -- foldl' (animate motion) state (reverse [1..number])
    foldl' (animate motion) state [1..number]
modelMotion' _ _ = undefined

modelMotion :: [Motion] -> State
modelMotion = foldl' modelMotion' (((0, 0), (0, 0)), [(0, 0)])
