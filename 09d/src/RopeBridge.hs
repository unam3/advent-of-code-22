module RopeBridge where


import Data.List (foldl', union)
import Data.Vector (Vector, (//), (!), elemIndex, fromList, minimumBy, maximumBy, slice, zip3)
import qualified Data.Vector as V (foldl')
import Prelude hiding (zip3)

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
animate (U _) state@(((hx, hy), (tx, ty)), tailVisitedAtLeastOnce) _
    -- consider 9 cases tail to head relations:
    --    - overlapping
    --    - touching:
    --        - to left, to right, to top, to bottom
    --        - diagonally adjacent:
    --            - diagonal NE, NW, SW, SE
    -- consult the `animate file`

    -- overlapping
    | (hx == tx && hy == ty)
        -- touching: head to the left from tail
        || (hx < tx && hy == ty)
        -- touching: head to the right from tail
        || (hx > tx && hy == ty)
        -- touching: head is below the tail
        || (hx == tx && hy < ty)
        -- touching (diagonally adjacent) head is to SW from tail
        || (hx < tx  && hy < ty)
        -- touching (diagonally adjacent) head is to SE from tail
        || (hx > tx && hy < ty)

    = (((hx, hy + 1), (tx, ty)), tailVisitedAtLeastOnce)
    
    -- touching: head is above the tail
    | hx == tx && hy > ty

    = let newTailCoords = (tx, ty + 1)
            in (((hx, hy + 1), newTailCoords), union tailVisitedAtLeastOnce [newTailCoords])

    -- touching (diagonally adjacent) head is to NE from tail
    | hx > tx && hy > ty

    = let newTailCoords = (tx + 1, ty + 1)
            in (((hx, hy + 1), newTailCoords), union tailVisitedAtLeastOnce [newTailCoords])

    -- touching (diagonally adjacent) head is to NW from tail
    | hx < tx  && hy > ty

    = let newTailCoords = (tx - 1, ty + 1)
        in (((hx, hy + 1), newTailCoords), union tailVisitedAtLeastOnce [newTailCoords])
    
    | otherwise = error $ "animate U: unexpected head/tail configuration: " ++ show state

animate (D _) state@(((hx, hy), (tx, ty)), tailVisitedAtLeastOnce) _
    -- overlapping
    | (hx == tx && hy == ty)
        -- touching: head to the left from tail
        || (hx < tx && hy == ty)
        -- touching: head to the right from tail
        || (hx > tx && hy == ty)
        -- touching: head is above the tail
        || (hx == tx && hy > ty)
        -- touching (diagonally adjacent) head is to NE from tail
        || (hx > tx && hy > ty)
        -- touching (diagonally adjacent) head is to NW from tail
        || (hx < tx  && hy > ty)

    = (((hx, hy - 1), (tx, ty)), tailVisitedAtLeastOnce)

    -- touching: head is below the tail
    | hx == tx && hy < ty

    = let newTailCoords = (tx, ty - 1)
            in (((hx, hy - 1), newTailCoords), union tailVisitedAtLeastOnce [newTailCoords])

    -- touching (diagonally adjacent) head is to SW from tail
    | hx < tx  && hy < ty

    = let newTailCoords = (tx - 1, ty - 1)
            in (((hx, hy - 1), newTailCoords), union tailVisitedAtLeastOnce [newTailCoords])

    -- touching (diagonally adjacent) head is to SE from tail
    | hx > tx && hy < ty

    = let newTailCoords = (tx + 1, ty - 1)
            in (((hx, hy - 1), newTailCoords), union tailVisitedAtLeastOnce [newTailCoords])

    | otherwise = error $ "animate D: unexpected head/tail configuration: " ++ show state

animate (L _) state@(((hx, hy), (tx, ty)), tailVisitedAtLeastOnce) _

    -- overlapping
    | (hx == tx && hy == ty)
        -- touching: head to the right from tail
        || (hx > tx && hy == ty)
        -- touching: head is above the tail
        || (hx == tx && hy > ty)
        -- touching: head is below the tail
        || (hx == tx && hy < ty)
        -- touching (diagonally adjacent) head is to NE from tail
        || (hx > tx && hy > ty)
        -- touching (diagonally adjacent) head is to SE from tail
        || (hx > tx && hy < ty)

    = (((hx - 1, hy), (tx, ty)), tailVisitedAtLeastOnce)

    -- touching: head to the left from tail
    |  hx < tx && hy == ty

    = let newTailCoords = (tx - 1, ty)
        in (((hx - 1, hy), newTailCoords), union tailVisitedAtLeastOnce [newTailCoords])

    -- touching (diagonally adjacent) head is to NW from tail
    | hx < tx  && hy > ty

    = let newTailCoords = (tx - 1, ty + 1)
        in (((hx - 1, hy), newTailCoords), union tailVisitedAtLeastOnce [newTailCoords])

    -- touching (diagonally adjacent) head is to SW from tail
    | hx < tx  && hy < ty

    = let newTailCoords = (tx - 1, ty - 1)
        in (((hx - 1, hy), newTailCoords), union tailVisitedAtLeastOnce [newTailCoords])

    | otherwise = error $ "animate L: unexpected head/tail configuration: " ++ show state

animate (R _) state@(((hx, hy), (tx, ty)), tailVisitedAtLeastOnce) _

    -- overlapping
    | (hx == tx && hy == ty)
        -- touching: head to the left from tail
         || (hx < tx && hy == ty)
        -- touching: head to the right from tail
        -- || (hx > tx && hy == ty)
        -- touching: head is above the tail
        || (hx == tx && hy > ty)
        -- touching: head is below the tail
        || (hx == tx && hy < ty)
        -- touching (diagonally adjacent) head is to NE from tail
        -- || (hx > tx && hy > ty)
        -- touching (diagonally adjacent) head is to NW from tail
        || (hx < tx  && hy > ty)
        -- touching (diagonally adjacent) head is to SW from tail
        || (hx < tx  && hy < ty)
        -- touching (diagonally adjacent) head is to SE from tail
        -- || (hx > tx && hy < ty)

    = (((hx + 1, hy), (tx, ty)), tailVisitedAtLeastOnce)

    -- touching: head to the right from tail
    |  hx > tx && hy == ty

    = let newTailCoords = (tx + 1, ty)
        in (((hx + 1, hy), newTailCoords), union tailVisitedAtLeastOnce [newTailCoords])

    -- touching (diagonally adjacent) head is to NE from tail
    | hx > tx && hy > ty

    = let newTailCoords = (tx + 1, ty + 1)
        in (((hx + 1, hy), newTailCoords), union tailVisitedAtLeastOnce [newTailCoords])

    -- touching (diagonally adjacent) head is to SE from tail
    | hx > tx && hy < ty

    = let newTailCoords = (tx + 1, ty - 1)
        in (((hx + 1, hy), newTailCoords), union tailVisitedAtLeastOnce [newTailCoords])

    | otherwise = error $ "animate R: unexpected head/tail configuration: " ++ show state

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

getDifference :: Int -> Int -> Int
getDifference = (abs .) . subtract

isAdjacentOrOverlapped :: Int -> Int -> Bool
--isAdjacentOrOverlapped coord coord1 = (<= 1) $ getDifference coord coord1
isAdjacentOrOverlapped = ((<= 1) .) . getDifference

follow :: Coords -> Coords -> Coords
follow = undefined

vanimate' :: Motion -> Int -> VState -> Int -> VState
-- rhx — relative head x
vanimate' (U numberOfSteps) stepNumber (knotsV, tailVisitedAtLeastOnce) rHeadPairIndex =

    let (rhx, rhy) = (!) knotsV rHeadPairIndex
        (rtx, rty) = (!) knotsV $ rHeadPairIndex + 1

    -- head always moves
    in if rHeadPairIndex == 0

    -- moving head
    then let movedHeadKnotsV = (//) knotsV [(rHeadPairIndex, (rhx, rhy + 1))]

        -- make move if new rhy and rty not adjacent after head move
        in if isAdjacentOrOverlapped rhx rtx && isAdjacentOrOverlapped (rhy + 1) rty

            then (movedHeadKnotsV, tailVisitedAtLeastOnce)
            
            else if rhx == rtx && rhy > rty

                then ((//) movedHeadKnotsV [(rHeadPairIndex + 1, (rtx, rty + 1))], tailVisitedAtLeastOnce)
                
                else if rhx > rtx && rhy > rty
                
                    then ((//) movedHeadKnotsV [(rHeadPairIndex + 1, (rtx + 1, rty + 1))], tailVisitedAtLeastOnce)

                    else if rhx < rtx  && rhy > rty

                        then ((//) movedHeadKnotsV [(rHeadPairIndex + 1, (rtx - 1, rty + 1))], tailVisitedAtLeastOnce)

                        else error $ show (rhx, rtx,  rhy, rty, isAdjacentOrOverlapped rhx rtx, isAdjacentOrOverlapped (rhy + 1) rty)

    --  knot pairs without head
    else let pairHasTail = rHeadPairIndex == 9

        in if isAdjacentOrOverlapped rhx rtx && isAdjacentOrOverlapped rhy rty

            then (knotsV, tailVisitedAtLeastOnce)
            
            else if rhy > rty && rhx == rtx

                then
                    let newTailCoords = (rtx, rty + 1)
                        tailVisitedAtLeastOnce' =
                            if pairHasTail
                            then union tailVisitedAtLeastOnce [newTailCoords]
                            else tailVisitedAtLeastOnce
                        newKnotsV = (//) knotsV [(rHeadPairIndex + 1, newTailCoords)]
                    in (newKnotsV, tailVisitedAtLeastOnce')
                
                else if rhx > rtx && rhy > rty
                
                    then
                        let newTailCoords = (rtx + 1, rty + 1)
                            tailVisitedAtLeastOnce' =
                                if pairHasTail
                                then union tailVisitedAtLeastOnce [newTailCoords]
                                else tailVisitedAtLeastOnce
                            newKnotsV = (//) knotsV [(rHeadPairIndex + 1, newTailCoords)]
                        in (newKnotsV, tailVisitedAtLeastOnce')

                    else if rhx < rtx  && rhy > rty

                        then
                            let newTailCoords = (rtx - 1, rty + 1)
                                tailVisitedAtLeastOnce' =
                                    if pairHasTail
                                    then union tailVisitedAtLeastOnce [newTailCoords]
                                    else tailVisitedAtLeastOnce
                                newKnotsV = (//) knotsV [(rHeadPairIndex + 1, newTailCoords)]
                            in (newKnotsV, tailVisitedAtLeastOnce')

                        else error $ show (rhx, rtx,  rhy, rty, isAdjacentOrOverlapped rhx rtx, isAdjacentOrOverlapped rhy rty)


vanimate' (D numberOfSteps) stepNumber (knotsV, tailVisitedAtLeastOnce) rHeadPairIndex =

    let (rhx, rhy) = (!) knotsV rHeadPairIndex
        (rtx, rty) = (!) knotsV $ rHeadPairIndex + 1

    -- head always moves
    in if rHeadPairIndex == 0

    -- moving head
    then let movedHeadKnotsV = (//) knotsV [(rHeadPairIndex, (rhx, rhy - 1))]

        -- make move if new rhy and rty not adjacent after head move
        in if isAdjacentOrOverlapped rhx rtx && isAdjacentOrOverlapped (rhy - 1) rty

            then (movedHeadKnotsV, tailVisitedAtLeastOnce)
            
            -- touching: head is below the tail
            else if rhy < rty && rhx == rtx

                then ((//) movedHeadKnotsV [(rHeadPairIndex + 1, (rtx, rty - 1))], tailVisitedAtLeastOnce)
                
                -- touching (diagonally adjacent) head is to SW from tail
                else if rhx < rtx && rhy < rty
                
                    then ((//) movedHeadKnotsV [(rHeadPairIndex + 1, (rtx - 1, rty - 1))], tailVisitedAtLeastOnce)

                    -- touching (diagonally adjacent) head is to SE from tail
                    else if rhx > rtx  && rhy < rty

                        then ((//) movedHeadKnotsV [(rHeadPairIndex + 1, (rtx + 1, rty - 1))], tailVisitedAtLeastOnce)

                        else error $ show (rhx, rtx,  rhy, rty, isAdjacentOrOverlapped rhx rtx, isAdjacentOrOverlapped (rhy - 1) rty)

    --  knot pairs without head
    else let pairHasTail = rHeadPairIndex == 9

        in if isAdjacentOrOverlapped rhx rtx && isAdjacentOrOverlapped rhy rty

            then (knotsV, tailVisitedAtLeastOnce)
            
            -- touching: head is below the tail
            else if rhy < rty && rhx == rtx

                then
                    let newTailCoords = (rtx, rty - 1)
                        tailVisitedAtLeastOnce' =
                            if pairHasTail
                            then union tailVisitedAtLeastOnce [newTailCoords]
                            else tailVisitedAtLeastOnce
                        newKnotsV = (//) knotsV [(rHeadPairIndex + 1, newTailCoords)]
                    in (newKnotsV, tailVisitedAtLeastOnce')

                -- touching (diagonally adjacent) head is to SW from tail
                else if rhx < rtx && rhy < rty
                
                    then
                        let newTailCoords = (rtx - 1, rty - 1)
                            tailVisitedAtLeastOnce' =
                                if pairHasTail
                                then union tailVisitedAtLeastOnce [newTailCoords]
                                else tailVisitedAtLeastOnce
                            newKnotsV = (//) knotsV [(rHeadPairIndex + 1, newTailCoords)]
                        in (newKnotsV, tailVisitedAtLeastOnce')

                    -- touching (diagonally adjacent) head is to SE from tail
                    else if rhx > rtx  && rhy < rty

                        then
                            let newTailCoords = (rtx + 1, rty - 1)
                                tailVisitedAtLeastOnce' =
                                    if pairHasTail
                                    then union tailVisitedAtLeastOnce [newTailCoords]
                                    else tailVisitedAtLeastOnce
                                newKnotsV = (//) knotsV [(rHeadPairIndex + 1, newTailCoords)]
                            in (newKnotsV, tailVisitedAtLeastOnce')

                        else error $ show (rhx, rtx,  rhy, rty, "rhy - 1")

vanimate' (L numberOfSteps) stepNumber (knotsV, tailVisitedAtLeastOnce) rHeadPairIndex =

    let (rhx, rhy) = (!) knotsV rHeadPairIndex
        (rtx, rty) = (!) knotsV $ rHeadPairIndex + 1

    -- head always moves
    in if rHeadPairIndex == 0

    -- moving head
    then let movedHeadKnotsV = (//) knotsV [(rHeadPairIndex, (rhx - 1, rhy))]

        -- make move if new rhy and rty not adjacent after head move
        in if isAdjacentOrOverlapped (rhx - 1) rtx && isAdjacentOrOverlapped rhy rty

            then (movedHeadKnotsV, tailVisitedAtLeastOnce)
            
            -- touching: head to the left from tail
            else if rhy == rty && rhx < rtx

                then ((//) movedHeadKnotsV [(rHeadPairIndex + 1, (rtx - 1, rty))], tailVisitedAtLeastOnce)
                
                -- touching (diagonally adjacent) head is to NW from tail
                else if rhx < rtx && rhy > rty
                
                    then ((//) movedHeadKnotsV [(rHeadPairIndex + 1, (rtx - 1, rty + 1))], tailVisitedAtLeastOnce)

                    -- touching (diagonally adjacent) head is to SW from tail
                    else if rhx < rtx  && rhy < rty

                        then ((//) movedHeadKnotsV [(rHeadPairIndex + 1, (rtx - 1, rty - 1))], tailVisitedAtLeastOnce)

                        else error $ show (rhx, rtx,  rhy, rty, "L error head pair", rHeadPairIndex)

    --  knot pairs without head
    else let pairHasTail = rHeadPairIndex == 9

        in if isAdjacentOrOverlapped rhx rtx && isAdjacentOrOverlapped rhy rty

            then (knotsV, tailVisitedAtLeastOnce)

            -- touching: head to the left from tail
            else if rhy == rty && rhx < rtx

                then
                    let newTailCoords = (rtx - 1, rty)
                        tailVisitedAtLeastOnce' =
                            if pairHasTail
                            then union tailVisitedAtLeastOnce [newTailCoords]
                            else tailVisitedAtLeastOnce
                        newKnotsV = (//) knotsV [(rHeadPairIndex + 1, newTailCoords)]
                    in (newKnotsV, tailVisitedAtLeastOnce')
                
                -- touching (diagonally adjacent) head is to NW from tail
                else if rhx < rtx && rhy > rty
                
                    then
                        let newTailCoords = (rtx - 1, rty + 1)
                            tailVisitedAtLeastOnce' =
                                if pairHasTail
                                then union tailVisitedAtLeastOnce [newTailCoords]
                                else tailVisitedAtLeastOnce
                            newKnotsV = (//) knotsV [(rHeadPairIndex + 1, newTailCoords)]
                        in (newKnotsV, tailVisitedAtLeastOnce')

                    -- touching (diagonally adjacent) head is to SW from tail
                    else if rhx < rtx  && rhy < rty

                        then
                            let newTailCoords = (rtx - 1, rty - 1)
                                tailVisitedAtLeastOnce' =
                                    if pairHasTail
                                    then union tailVisitedAtLeastOnce [newTailCoords]
                                    else tailVisitedAtLeastOnce
                                newKnotsV = (//) knotsV [(rHeadPairIndex + 1, newTailCoords)]
                            in (newKnotsV, tailVisitedAtLeastOnce')

                        else error $ show (rhx, rtx,  rhy, rty, "L error", rHeadPairIndex, show knotsV)
            
vanimate' (R numberOfSteps) stepNumber (knotsV, tailVisitedAtLeastOnce) rHeadPairIndex =

    let (rhx, rhy) = (!) knotsV rHeadPairIndex
        (rtx, rty) = (!) knotsV $ rHeadPairIndex + 1

    -- head always moves
    in if rHeadPairIndex == 0

    -- moving head
    then let movedHeadKnotsV = (//) knotsV [(rHeadPairIndex, (rhx + 1, rhy))]

        -- make move if new rhy and rty not adjacent after head move
        in if isAdjacentOrOverlapped (rhx + 1) rtx && isAdjacentOrOverlapped rhy rty

            then (movedHeadKnotsV, tailVisitedAtLeastOnce)
            
            -- touching: head to the right from tail
            else if rhy == rty && rhx > rtx

                then ((//) movedHeadKnotsV [(rHeadPairIndex + 1, (rtx + 1, rty))], tailVisitedAtLeastOnce)

                -- touching (diagonally adjacent) head is to NE from tail
                else if rhx > rtx && rhy > rty
                
                    then ((//) movedHeadKnotsV [(rHeadPairIndex + 1, (rtx + 1, rty + 1))], tailVisitedAtLeastOnce)

                    -- touching (diagonally adjacent) head is to SE from tail
                    else if rhx > rtx  && rhy < rty

                        then ((//) movedHeadKnotsV [(rHeadPairIndex + 1, (rtx + 1, rty - 1))], tailVisitedAtLeastOnce)

                        else error $ show (rhx, rtx,  rhy, rty, "rhx + 1")

    --  knot pairs without head
    else let pairHasTail = rHeadPairIndex == 9

        in if isAdjacentOrOverlapped rhx rtx && isAdjacentOrOverlapped rhy rty

            then (knotsV, tailVisitedAtLeastOnce)
            
            -- touching: head to the right from tail
            else if rhy == rty && rhx > rtx

                then
                    let newTailCoords = (rtx + 1, rty)
                        tailVisitedAtLeastOnce' =
                            if pairHasTail
                            then union tailVisitedAtLeastOnce [newTailCoords]
                            else tailVisitedAtLeastOnce
                        newKnotsV = (//) knotsV [(rHeadPairIndex + 1, newTailCoords)]
                    in (newKnotsV, tailVisitedAtLeastOnce')

                -- touching (diagonally adjacent) head is to NE from tail
                else if rhx > rtx && rhy > rty
                
                    then
                        let newTailCoords = (rtx + 1, rty + 1)
                            tailVisitedAtLeastOnce' =
                                if pairHasTail
                                then union tailVisitedAtLeastOnce [newTailCoords]
                                else tailVisitedAtLeastOnce
                            newKnotsV = (//) knotsV [(rHeadPairIndex + 1, newTailCoords)]
                        in (newKnotsV, tailVisitedAtLeastOnce')

                    -- touching (diagonally adjacent) head is to SE from tail
                    else if rhx > rtx  && rhy < rty

                        then
                            let newTailCoords = (rtx + 1, rty - 1)
                                tailVisitedAtLeastOnce' =
                                    if pairHasTail
                                    then union tailVisitedAtLeastOnce [newTailCoords]
                                    else tailVisitedAtLeastOnce
                                newKnotsV = (//) knotsV [(rHeadPairIndex + 1, newTailCoords)]
                            in (newKnotsV, tailVisitedAtLeastOnce')

                        else error $ show (rhx, rtx,  rhy, rty, "rhx + 1")


vanimate :: Motion -> VState -> Int -> VState
vanimate motion state@(knotsCoords, _) stepNumber =
    let rHeadPairIndexes = fromList [0..8]
    in foldl'
        (vanimate' motion stepNumber)
        state
        rHeadPairIndexes

vmodelMotion' :: VState -> Motion -> VState
vmodelMotion' state motion =
    let lastStepNumber = getNumberOfSteps motion
    in foldl' (vanimate motion) state $ reverse [1..lastStepNumber]

vmodelMotion :: [Motion] -> VState
vmodelMotion =
    foldl'
        vmodelMotion'
        (
            fromList $ replicate 10 (0, 0),
            [(0, 0)]
        )


labelOrDot :: Maybe Int -> String
labelOrDot Nothing = "."
labelOrDot (Just 0) = "H"
labelOrDot (Just n) = show n

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
    in plot (minX, maxX, minY, maxY) (labelOrDot . (`elemIndex` knotsCoords))
