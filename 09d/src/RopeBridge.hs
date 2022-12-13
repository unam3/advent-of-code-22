module RopeBridge where


import Data.List (foldl', union)
import Data.Vector (Vector, (//), elem, fromList, minimumBy, maximumBy, slice, zip3)
import qualified Data.Vector as V (foldl')
import Prelude hiding (elem, zip3)

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
    else if hx > tx && hy > ty
    then let newTailCoords = (tx + 1, ty + 1)
        in (((hx, hy + 1), newTailCoords), union tailVisitedAtLeastOnce [newTailCoords])
    -- touching (diagonally adjacent) head is to NW from tail
    else if hx < tx  && hy > ty
    then let newTailCoords = (tx - 1, ty + 1)
        in (((hx, hy + 1), newTailCoords), union tailVisitedAtLeastOnce [newTailCoords])
    -- touching (diagonally adjacent) head is to SW from tail
    else if hx < tx  && hy < ty
    then (((hx, hy + 1), (tx, ty)), tailVisitedAtLeastOnce)
    -- touching (diagonally adjacent) head is to SE from tail
    else if hx > tx && hy < ty
    then (((hx, hy + 1), (tx, ty)), tailVisitedAtLeastOnce)
    else error $ "animate U: unexpected head/tail configuration: " ++ show state

animate (D _) state@(((hx, hy), (tx, ty)), tailVisitedAtLeastOnce) _ =
    -- overlapping
    if hx == tx && hy == ty
    then (((hx, hy - 1), (tx, ty)), tailVisitedAtLeastOnce)
    -- touching: head to the right from tail
    else if hy == ty && hx < tx
    then (((hx, hy - 1), (tx, ty)), tailVisitedAtLeastOnce)
    -- touching: head to the left from tail
    else if hy == ty && hx > tx
    then (((hx, hy - 1), (tx, ty)), tailVisitedAtLeastOnce)
    -- touching: head is below the tail
    else if hy < ty && hx == tx
    then let newTailCoords = (tx, ty - 1)
        in (((hx, hy - 1), newTailCoords), union tailVisitedAtLeastOnce [newTailCoords])
    -- touching: head is above the tail
    else if hy > ty && hx == tx
    then (((hx, hy - 1), (tx, ty)), tailVisitedAtLeastOnce)
    -- touching (diagonally adjacent) head is to NE from tail
    else if hx > tx && hy > ty
    then (((hx, hy - 1), (tx, ty)), tailVisitedAtLeastOnce)
    -- touching (diagonally adjacent) head is to NW from tail
    else if hx < tx  && hy > ty
    then (((hx, hy - 1), (tx, ty)), tailVisitedAtLeastOnce)
    -- touching (diagonally adjacent) head is to SW from tail
    else if hx < tx  && hy < ty
    then let newTailCoords = (tx - 1, ty - 1)
        in (((hx, hy - 1), newTailCoords), union tailVisitedAtLeastOnce [newTailCoords])
    -- touching (diagonally adjacent) head is to SE from tail
    else if hx > tx && hy < ty
    then let newTailCoords = (tx + 1, ty - 1)
        in (((hx, hy - 1), newTailCoords), union tailVisitedAtLeastOnce [newTailCoords])
    else error $ "animate D: unexpected head/tail configuration: " ++ show state

animate (L _) state@(((hx, hy), (tx, ty)), tailVisitedAtLeastOnce) _ =
    -- overlapping
    if hx == tx && hy == ty
    then (((hx - 1, hy), (tx, ty)), tailVisitedAtLeastOnce)
    -- touching: head to the right from tail
    else if hy == ty && hx > tx
    then (((hx - 1, hy), (tx, ty)), tailVisitedAtLeastOnce)
    -- touching: head to the left from tail
    else if hy == ty && hx < tx
    then let newTailCoords = (tx - 1, ty)
        in (((hx - 1, hy), newTailCoords), union tailVisitedAtLeastOnce [newTailCoords])
    -- touching: head is below the tail
    else if hy < ty && hx == tx
    then (((hx - 1, hy), (tx, ty)), tailVisitedAtLeastOnce)
    -- touching: head is above the tail
    else if hy > ty && hx == tx
    then (((hx - 1, hy), (tx, ty)), tailVisitedAtLeastOnce)
    -- touching (diagonally adjacent) head is to NE from tail
    else if hx > tx && hy > ty
    then (((hx - 1, hy), (tx, ty)), tailVisitedAtLeastOnce)
    -- touching (diagonally adjacent) head is to NW from tail
    else if hx < tx  && hy > ty
    then let newTailCoords = (tx - 1, ty + 1)
        in (((hx - 1, hy), newTailCoords), union tailVisitedAtLeastOnce [newTailCoords])
    -- touching (diagonally adjacent) head is to SW from tail
    else if hx < tx  && hy < ty
    then let newTailCoords = (tx - 1, ty - 1)
        in (((hx - 1, hy), newTailCoords), union tailVisitedAtLeastOnce [newTailCoords])
    -- touching (diagonally adjacent) head is to SE from tail
    else if hx > tx && hy < ty
    then (((hx - 1, hy), (tx, ty)), tailVisitedAtLeastOnce)
    else error $ "animate L: unexpected head/tail configuration: " ++ show state

animate (R _) state@(((hx, hy), (tx, ty)), tailVisitedAtLeastOnce) _ =
    -- overlapping
    if hx == tx && hy == ty
    then (((hx + 1, hy), (tx, ty)), tailVisitedAtLeastOnce)
    -- touching: head to the right from tail
    else if hy == ty && hx > tx
    then let newTailCoords = (tx + 1, ty)
        in (((hx + 1, hy), newTailCoords), union tailVisitedAtLeastOnce [newTailCoords])
    -- touching: head to the left from tail
    else if hy == ty && hx < tx
    then (((hx + 1, hy), (tx, ty)), tailVisitedAtLeastOnce)
    -- touching: head is below the tail
    else if hy < ty && hx == tx
    then (((hx + 1, hy), (tx, ty)), tailVisitedAtLeastOnce)
    -- touching: head is above the tail
    else if hy > ty && hx == tx
    then (((hx + 1, hy), (tx, ty)), tailVisitedAtLeastOnce)
    -- touching (diagonally adjacent) head is to NE from tail
    else if hx > tx && hy > ty
    then let newTailCoords = (tx + 1, ty + 1)
        in (((hx + 1, hy), newTailCoords), union tailVisitedAtLeastOnce [newTailCoords])
    -- touching (diagonally adjacent) head is to NW from tail
    else if hx < tx  && hy > ty
    then (((hx + 1, hy), (tx, ty)), tailVisitedAtLeastOnce)
    -- touching (diagonally adjacent) head is to SW from tail
    else if hx < tx  && hy < ty
    then (((hx + 1, hy), (tx, ty)), tailVisitedAtLeastOnce)
    -- touching (diagonally adjacent) head is to SE from tail
    else if hx > tx && hy < ty
    then let newTailCoords = (tx + 1, ty - 1)
        in (((hx + 1, hy), newTailCoords), union tailVisitedAtLeastOnce [newTailCoords])
    else error $ "animate R: unexpected head/tail configuration: " ++ show state

getNumberOfSteps :: Motion -> Int
getNumberOfSteps (U n) = n
getNumberOfSteps (D n) = n
getNumberOfSteps (L n) = n
getNumberOfSteps (R n) = n

modelMotion' :: State -> Motion -> State
modelMotion' state motion =
    -- for debug
    -- foldl' (animate motion) state (reverse [1..number])
    foldl' (animate motion) state [1..getNumberOfSteps motion]

modelMotion :: [Motion] -> State
modelMotion = foldl' modelMotion' (((0, 0), (0, 0)), [(0, 0)])


type KnotsCoords = Vector Coords
type VState = (KnotsCoords, TailVisitedAtLeastOnce)

vanimate' :: Motion -> Int -> VState -> (Int, Coords, Coords) -> VState
-- rhx — relative head x
vanimate' (U numberOfSteps) stepNumber state@(knotsV, tailVisitedAtLeastOnce) (headIndex, (rhx, rhy), (rtx, rty)) =
    let pairHasTail = stepNumber == numberOfSteps
    -- overlapping
    in if rhx == rtx && rhy == rty
    then ((//) knotsV [(headIndex, (rhx, rhy + 1))], tailVisitedAtLeastOnce)
    -- touching: head to the right from tail
    else if rhy == rty && rhx < rtx
    then ((//) knotsV [(headIndex, (rhx, rhy + 1))], tailVisitedAtLeastOnce)
    -- touching: head to the left from tail
    else if rhy == rty && rhx > rtx
    then ((//) knotsV [(headIndex, (rhx, rhy + 1))], tailVisitedAtLeastOnce)
    -- touching: head is below the tail
    else if rhy < rty && rhx == rtx
    then ((//) knotsV [(headIndex, (rhx, rhy + 1))], tailVisitedAtLeastOnce)
    -- touching: head is above the tail
    else if rhy > rty && rhx == rtx
    then if pairHasTail
        then let newTailCoords = (rtx, rty + 1)
            in (
                (//) knotsV [(headIndex, (rhx, rhy + 1)), (headIndex + 1, newTailCoords)],
                union tailVisitedAtLeastOnce [newTailCoords]
            )
        else ((//) knotsV [(headIndex, (rhx, rhy + 1))], tailVisitedAtLeastOnce)
    -- touching (diagonally adjacent) head is to NE from tail
    else if rhx > rtx && rhy > rty
    then if pairHasTail
        then let newTailCoords = (rtx + 1, rty + 1)
            in (
                (//) knotsV [(headIndex, (rhx, rhy + 1)), (headIndex + 1, newTailCoords)],
                union tailVisitedAtLeastOnce [newTailCoords]
            )
        else ((//) knotsV [(headIndex, (rhx, rhy + 1))], tailVisitedAtLeastOnce)
    -- touching (diagonally adjacent) head is to NW from tail
    else if rhx < rtx  && rhy > rty
    then if pairHasTail
        then let newTailCoords = (rtx - 1, rty + 1)
            in (
                (//) knotsV [(headIndex, (rhx, rhy + 1)), (headIndex + 1, newTailCoords)],
                union tailVisitedAtLeastOnce [newTailCoords]
            )
        else ((//) knotsV [(headIndex, (rhx, rhy + 1))], tailVisitedAtLeastOnce)
    -- touching (diagonally adjacent) head is to SW from tail
    else if rhx < rtx  && rhy < rty
    then ((//) knotsV [(headIndex, (rhx, rhy + 1))], tailVisitedAtLeastOnce)
    -- touching (diagonally adjacent) head is to SE from tail
    else if rhx > rtx && rhy < rty
    then ((//) knotsV [(headIndex, (rhx, rhy + 1))], tailVisitedAtLeastOnce)
    else error $ "animate U: unexpected head/tail configuration: " ++ show state

vanimate' (D numberOfSteps) stepNumber state@(knotsV, tailVisitedAtLeastOnce) (headIndex, (rhx, rhy), (rtx, rty)) =
    let pairHasTail = stepNumber == numberOfSteps
    -- overlapping
    in if rhx == rtx && rhy == rty
    then ((//) knotsV [(headIndex, (rhx, rhy - 1))], tailVisitedAtLeastOnce)
    -- touching: head to the right from tail
    else if rhy == rty && rhx < rtx
    then ((//) knotsV [(headIndex, (rhx, rhy - 1))], tailVisitedAtLeastOnce)
    -- touching: head to the left from tail
    else if rhy == rty && rhx > rtx
    then ((//) knotsV [(headIndex, (rhx, rhy - 1))], tailVisitedAtLeastOnce)
    -- touching: head is below the tail
    else if rhy < rty && rhx == rtx
    then if pairHasTail
        then let newTailCoords = (rtx, rty - 1)
            in (
                (//) knotsV [(headIndex, (rhx, rhy - 1)), (headIndex + 1, newTailCoords)],
                union tailVisitedAtLeastOnce [newTailCoords]
            )
        else ((//) knotsV [(headIndex, (rhx, rhy - 1))], tailVisitedAtLeastOnce)
    -- touching: head is above the tail
    else if rhy > rty && rhx == rtx
    then ((//) knotsV [(headIndex, (rhx, rhy - 1))], tailVisitedAtLeastOnce)
    -- touching (diagonally adjacent) head is to NE from tail
    else if rhx > rtx && rhy > rty
    then ((//) knotsV [(headIndex, (rhx, rhy - 1))], tailVisitedAtLeastOnce)
    -- touching (diagonally adjacent) head is to NW from tail
    else if rhx < rtx  && rhy > rty
    then ((//) knotsV [(headIndex, (rhx, rhy - 1))], tailVisitedAtLeastOnce)
    -- touching (diagonally adjacent) head is to SW from tail
    else if rhx < rtx  && rhy < rty
    then if pairHasTail
        then let newTailCoords = (rtx - 1, rty - 1)
            in (
                (//) knotsV [(headIndex, (rhx, rhy - 1)), (headIndex + 1, newTailCoords)],
                union tailVisitedAtLeastOnce [newTailCoords]
            )
        else ((//) knotsV [(headIndex, (rhx, rhy - 1))], tailVisitedAtLeastOnce)
    -- touching (diagonally adjacent) head is to SE from tail
    else if rhx > rtx && rhy < rty
    then if pairHasTail
        then let newTailCoords = (rtx + 1, rty - 1)
            in (
                (//) knotsV [(headIndex, (rhx, rhy - 1)), (headIndex + 1, newTailCoords)],
                union tailVisitedAtLeastOnce [newTailCoords]
            )
        else ((//) knotsV [(headIndex, (rhx, rhy - 1))], tailVisitedAtLeastOnce)
    else error $ "animate D: unexpected head/tail configuration: " ++ show state

vanimate' (L numberOfSteps) stepNumber state@(knotsV, tailVisitedAtLeastOnce) (headIndex, (rhx, rhy), (rtx, rty)) =
    let pairHasTail = stepNumber == numberOfSteps
    -- overlapping
    in if rhx == rtx && rhy == rty
    then ((//) knotsV [(headIndex, (rhx - 1, rhy))], tailVisitedAtLeastOnce)
    -- touching: head to the right from tail
    else if rhy == rty && rhx > rtx
    then ((//) knotsV [(headIndex, (rhx - 1, rhy))], tailVisitedAtLeastOnce)
    -- touching: head to the left from tail
    else if rhy == rty && rhx < rtx
    then if pairHasTail
        then let newTailCoords = (rtx - 1, rty)
            in (
                (//) knotsV [(headIndex, (rhx - 1, rhy)), (headIndex + 1, newTailCoords)],
                union tailVisitedAtLeastOnce [newTailCoords]
            )
        else ((//) knotsV [(headIndex, (rhx - 1, rhy))], tailVisitedAtLeastOnce)
    -- touching: head is below the tail
    else if rhy < rty && rhx == rtx
    then ((//) knotsV [(headIndex, (rhx - 1, rhy))], tailVisitedAtLeastOnce)
    -- touching: head is above the tail
    else if rhy > rty && rhx == rtx
    then ((//) knotsV [(headIndex, (rhx - 1, rhy))], tailVisitedAtLeastOnce)
    -- touching (diagonally adjacent) head is to NE from tail
    else if rhx > rtx && rhy > rty
    then ((//) knotsV [(headIndex, (rhx - 1, rhy))], tailVisitedAtLeastOnce)
    -- touching (diagonally adjacent) head is to NW from tail
    else if rhx < rtx  && rhy > rty
    then if pairHasTail
        then let newTailCoords = (rtx - 1, rty + 1)
            in (
                (//) knotsV [(headIndex, (rhx - 1, rhy)), (headIndex + 1, newTailCoords)],
                union tailVisitedAtLeastOnce [newTailCoords]
            )
        else ((//) knotsV [(headIndex, (rhx - 1, rhy))], tailVisitedAtLeastOnce)
    -- touching (diagonally adjacent) head is to SW from tail
    else if rhx < rtx  && rhy < rty
    then if pairHasTail
        then let newTailCoords = (rtx - 1, rty - 1)
            in (
                (//) knotsV [(headIndex, (rhx - 1, rhy)), (headIndex + 1, newTailCoords)],
                union tailVisitedAtLeastOnce [newTailCoords]
            )
        else ((//) knotsV [(headIndex, (rhx - 1, rhy))], tailVisitedAtLeastOnce)
    -- touching (diagonally adjacent) head is to SE from tail
    else if rhx > rtx && rhy < rty
    then ((//) knotsV [(headIndex, (rhx - 1, rhy))], tailVisitedAtLeastOnce)
    else error $ "animate L: unexpected head/tail configuration: " ++ show state

vanimate' (R numberOfSteps) stepNumber state@(knotsV, tailVisitedAtLeastOnce) (headIndex, (rhx, rhy), (rtx, rty)) =
    let pairHasTail = stepNumber == numberOfSteps
    -- overlapping
    in if rhx == rtx && rhy == rty
    then ((//) knotsV [(headIndex, (rhx + 1, rhy))], tailVisitedAtLeastOnce)
    -- touching: head to the right from tail
    else if rhy == rty && rhx > rtx
    then if pairHasTail
        then let newTailCoords = (rtx + 1, rty)
            in (
                (//) knotsV [(headIndex, (rhx + 1, rhy)), (headIndex + 1, newTailCoords)],
                union tailVisitedAtLeastOnce [newTailCoords]
            )
        else ((//) knotsV [(headIndex, (rhx + 1, rhy))], tailVisitedAtLeastOnce)
    -- touching: head to the left from tail
    else if rhy == rty && rhx < rtx
    then ((//) knotsV [(headIndex, (rhx + 1, rhy))], tailVisitedAtLeastOnce)
    -- touching: head is below the tail
    else if rhy < rty && rhx == rtx
    then ((//) knotsV [(headIndex, (rhx + 1, rhy))], tailVisitedAtLeastOnce)
    -- touching: head is above the tail
    else if rhy > rty && rhx == rtx
    then ((//) knotsV [(headIndex, (rhx + 1, rhy))], tailVisitedAtLeastOnce)
    -- touching (diagonally adjacent) head is to NE from tail
    else if rhx > rtx && rhy > rty
    then if pairHasTail
        then let newTailCoords = (rtx + 1, rty + 1)
            in (
                (//) knotsV [(headIndex, (rhx + 1, rhy)), (headIndex + 1, newTailCoords)],
                union tailVisitedAtLeastOnce [newTailCoords]
            )
        else ((//) knotsV [(headIndex, (rhx + 1, rhy))], tailVisitedAtLeastOnce)
    -- touching (diagonally adjacent) head is to NW from tail
    else if rhx < rtx  && rhy > rty
    then ((//) knotsV [(headIndex, (rhx + 1, rhy))], tailVisitedAtLeastOnce)
    -- touching (diagonally adjacent) head is to SW from tail
    else if rhx < rtx  && rhy < rty
    then ((//) knotsV [(headIndex, (rhx + 1, rhy))], tailVisitedAtLeastOnce)
    -- touching (diagonally adjacent) head is to SE from tail
    else if rhx > rtx && rhy < rty
    then if pairHasTail
        then let newTailCoords = (rtx + 1, rty - 1)
            in (
                (//) knotsV [(headIndex, (rhx + 1, rhy)), (headIndex + 1, newTailCoords)],
                union tailVisitedAtLeastOnce [newTailCoords]
            )
        else ((//) knotsV [(headIndex, (rhx + 1, rhy))], tailVisitedAtLeastOnce)
    else error $ "animate R: unexpected head/tail configuration: " ++ show state

vanimate :: Motion -> VState -> Int -> VState
-- check if stepNumber modifications still holds up
vanimate motion state@(knotsCoords, _) stepNumber =
    let knotsToAnimateAtThisStep = slice 0 (stepNumber + 1) knotsCoords
        knotPairs = zip3
            (fromList [0..stepNumber - 1])
            knotsToAnimateAtThisStep
            $ slice 1 (stepNumber - 1) knotsToAnimateAtThisStep
    in V.foldl'
        (vanimate' motion stepNumber)
        state
        knotPairs

vmodelMotion' :: VState -> Motion -> VState
vmodelMotion' state motion =
    let lastStepNumber = getNumberOfSteps motion + 1
    in foldl' (vanimate motion) state $ reverse [1..lastStepNumber]

vmodelMotion :: [Motion] -> VState
vmodelMotion =
    foldl'
        vmodelMotion'
        (
            fromList $ replicate 10 (0, 0),
            [(0, 0)]
        )



sharpOrDot :: KnotsCoords -> (Int, Int) -> String
sharpOrDot knotsCoords (x, y) =
    if elem (x, y) knotsCoords
    then "#"
    else "."
    

plot :: (Int, Int, Int, Int) -> ((Int, Int) -> String) -> String
plot (minX, maxX, minY, maxY) f =
    foldl'
        (\ string y ->
             (foldl'
                (\ row x ->
                    --let toPlot = show (x, y)
                    let toPlot = f (x, y)
                    in row ++ toPlot
                )
                ""
                [minX..maxX]
            ) ++ "\n" ++ string
        )
        ""
        [minY..maxY]

visualize :: VState -> String
visualize (knotsCoords, _) = 
    -- find out min/max of x/y
    let minX = fst $ minimumBy (\ (x, _) (x1, _) -> compare x x1) knotsCoords
        maxX = fst $ maximumBy (\ (x, _) (x1, _) -> compare x x1) knotsCoords
        minY = snd $ minimumBy (\ (_, y) (_, y1) -> compare y y1) knotsCoords
        maxY = snd $ maximumBy (\ (_, y) (_, y1) -> compare y y1) knotsCoords
    in plot (minX, maxX, minY, maxY) (sharpOrDot knotsCoords)
