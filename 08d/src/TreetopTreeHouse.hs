module TreetopTreeHouse where

import qualified Data.List as L
import Data.Map.Strict (Map, (!), elems, filterWithKey, foldlWithKey', fromList)

type Coords = (Int, Int)
type Height = Int

type HeightMap = Map Coords Height

parseX :: Int -> String -> [((Int, Int), Height)]
parseX y = zip (fmap (\ x -> (x, y)) [1..]) . fmap ((read :: String -> Int) . (:[]))

parseInput :: String -> HeightMap
parseInput =
    fromList
        . concatMap (uncurry parseX)
        . zip [1..]
        . lines

findMaxX :: Int -> Coords -> Int -> Int
findMaxX acc (x, _) _ = max acc x

-- assuming that trees grid shape is square (which obvious from testInput, input.txt files) and minimal coordinate is 1
countTreesAroundEdge :: HeightMap -> Int
countTreesAroundEdge =
    (\ maxX ->
        let edgeTreesNumber = maxX - 1
        in edgeTreesNumber * 4
    ) . foldlWithKey' findMaxX 1


isTreeVisibleFromLeft :: HeightMap -> Coords -> Int -> Bool
isTreeVisibleFromLeft heigtMap (referenceX, referenceY) referenceTreeHeight =
    -- x coordinate should be shorter than right one (reference)
    L.foldl'
            (\ areAllTreesLower treeHeight ->
                areAllTreesLower && referenceTreeHeight > treeHeight
            )
            True
        -- sort them in right order: from right to left
        . reverse
        -- get heightmap in ascending order of keys
        . elems
        -- get all the elements height to the left from Coords
        $ filterWithKey (\ (x, y) _ -> referenceX > x && referenceY == y) heigtMap
    
isTreeVisibleFromRight :: HeightMap -> Coords -> Int -> Bool
isTreeVisibleFromRight heigtMap (referenceX, referenceY) referenceTreeHeight =
    -- x coordinate should decrease from left to right
    L.foldl'
            (\ areAllTreesLower treeHeight ->
                areAllTreesLower && referenceTreeHeight > treeHeight
            )
            True
        -- get heightmap in ascending order of keys
        . elems
        -- get all the elements height to the right from Coords
        $ filterWithKey (\ (x, y) _ -> referenceX < x && referenceY == y) heigtMap
    

part1Foldl'Params :: ((Int -> Bool -> Int -> Bool), Bool)
part1Foldl'Params =
    let foldingFunction =
            (\ referenceTreeHeight areAllTreesLower treeHeight ->
                areAllTreesLower && referenceTreeHeight > treeHeight
            )
        initialAccValue = True
    in (foldingFunction, initialAccValue)


isTreeVisibleFromTop :: HeightMap -> Coords -> Int -> Bool
isTreeVisibleFromTop heigtMap (referenceX, referenceY) referenceTreeHeight =
    -- y coordinate should decrease from bottom to top
    L.foldl'
            (\ areAllTreesLower treeHeight ->
                areAllTreesLower && referenceTreeHeight > treeHeight
            )
            True
        -- sort them in right order: from bottom to top
        . reverse
        -- get heightmap in ascending order of keys
        . elems
        -- get all the elements height to the top from Coords
        $ filterWithKey (\ (x, y) _ -> referenceY > y && referenceX == x) heigtMap

isTreeVisibleFromBottom :: HeightMap -> Coords -> Int -> Bool
isTreeVisibleFromBottom heigtMap (referenceX, referenceY) referenceTreeHeight =
    -- y coordinate should decrease from top to bottom
    L.foldl'
            (\ areAllTreesLower treeHeight ->
                areAllTreesLower && referenceTreeHeight > treeHeight
            )
            True
        -- get heightmap in ascending order of keys
        . elems
        -- get all the elements height to the top from Coords
        $ filterWithKey (\ (x, y) _ -> referenceY < y && referenceX == x) heigtMap

isInteriorTreeVisible :: HeightMap -> Coords -> Bool
isInteriorTreeVisible heightMap coords =
    let treeHeight = (!) heightMap coords
    -- consecutive search: left, up, right and down
    in isTreeVisibleFromLeft heightMap coords treeHeight
        || isTreeVisibleFromRight heightMap coords treeHeight
        || isTreeVisibleFromTop heightMap coords treeHeight
        || isTreeVisibleFromBottom heightMap coords treeHeight


-- A tree is visible if all of the other trees between it and an edge of the grid are shorter than it
findInteriorVisibleTrees :: HeightMap -> Int
findInteriorVisibleTrees heightMap =
    -- to exclude tree from the edges
    let minCoord = 2
        maxCoord = subtract 1
            $ foldlWithKey' findMaxX 2 heightMap
        interiorTreeCoords = concatMap (zip [minCoord .. maxCoord] . repeat) [minCoord..maxCoord]
    in length $ filter (isInteriorTreeVisible heightMap) interiorTreeCoords

countTreesVisibleFromOutside :: HeightMap -> Int
-- how we use point-free here? Applicative?
-- :t :: [HeightMap -> Int]
--countTreesVisibleFromOutside = sum . fmap [countTreesAroundEdge, findInteriorVisibleTrees]
countTreesVisibleFromOutside heightMap = sum [countTreesAroundEdge heightMap, findInteriorVisibleTrees heightMap]


-- (If a tree is right on the edge, at least one of its viewing distances will be zero.)
getScenicScoreFromLeft :: HeightMap -> Coords -> Int -> Int
getScenicScoreFromLeft heigtMap (referenceX, referenceY) referenceTreeHeight =
    snd .
        -- x coordinate should be less than right one (reference)
        L.foldl'
                (\ ((areAllTreesLower, hasEncounterSameHeightTree), visibleTreeCount) treeHeight ->
                    -- stop if you reach an edge
                    if areAllTreesLower
                        && not hasEncounterSameHeightTree
                        && referenceTreeHeight > treeHeight
                    then ((True, False), visibleTreeCount + 1)
                    -- or at the first tree that is the same height or taller than the tree under consideration.
                    else if not hasEncounterSameHeightTree
                        && referenceTreeHeight <= treeHeight
                    then ((False, True), visibleTreeCount + 1)
                    else ((False, True), visibleTreeCount)
                )
                ((True, False), 0)
        -- sort them in right order: from right to left
        . reverse
        -- get heightmap in ascending order of keys
        . elems
        -- get all the elements height to the left from Coords
        $ filterWithKey (\ (x, y) _ -> referenceX > x && referenceY == y) heigtMap

getScenicScoreFromRight :: HeightMap -> Coords -> Int -> Int
getScenicScoreFromRight heigtMap (referenceX, referenceY) referenceTreeHeight =
    snd . 
        -- x coordinate should decrease from left to right
        L.foldl'
                (\ ((areAllTreesLower, hasEncounterSameHeightTree), visibleTreeCount) treeHeight ->
                    -- stop if you reach an edge
                    if areAllTreesLower
                        && not hasEncounterSameHeightTree
                        && referenceTreeHeight > treeHeight
                    -- or at the first tree that is the same height or taller than the tree under consideration.
                    then ((True, False), visibleTreeCount + 1)
                    else if not hasEncounterSameHeightTree
                        && referenceTreeHeight <= treeHeight
                    then ((False, True), visibleTreeCount + 1)
                    else ((False, True), visibleTreeCount)
                )
                ((True, False), 0)
        -- get heightmap in ascending order of keys
        . elems
        -- get all the elements height to the right from Coords
        $ filterWithKey (\ (x, y) _ -> referenceX < x && referenceY == y) heigtMap
    

getScenicScoreFromTop :: HeightMap -> Coords -> Int -> Int
getScenicScoreFromTop heigtMap (referenceX, referenceY) referenceTreeHeight =
    snd . 
        -- y coordinate should decrease from bottom to top
        L.foldl'
                (\ (areAllTreesLower, visibleTreeCount) treeHeight ->
                    -- stop if you reach an edge
                    -- or at the first tree that is the same height or taller than the tree under consideration.
                    if areAllTreesLower && referenceTreeHeight > treeHeight
                    then (True, visibleTreeCount + 1)
                    else (False, visibleTreeCount)
                )
                (True, 0)
        -- sort them in right order: from bottom to top
        . reverse
        -- get heightmap in ascending order of keys
        . elems
        -- get all the elements height to the top from Coords
        $ filterWithKey (\ (x, y) _ -> referenceY > y && referenceX == x) heigtMap

getScenicScoreFromBottom :: HeightMap -> Coords -> Int -> Int
getScenicScoreFromBottom heigtMap (referenceX, referenceY) referenceTreeHeight =
    snd . 
        -- y coordinate should decrease from top to bottom
        L.foldl'
                (\ (areAllTreesLower, visibleTreeCount) treeHeight ->
                    -- stop if you reach an edge
                    -- or at the first tree that is the same height or taller than the tree under consideration.
                    if areAllTreesLower && referenceTreeHeight > treeHeight
                    then (True, visibleTreeCount + 1)
                    else (False, visibleTreeCount)
                )
                (True, 0)
        -- get heightmap in ascending order of keys
        . elems
        -- get all the elements height to the top from Coords
        $ filterWithKey (\ (x, y) _ -> referenceY < y && referenceX == x) heigtMap

getScenicScore :: HeightMap -> Coords -> [Int]
getScenicScore heightMap coords =
    let treeHeight = (!) heightMap coords
    --in product [
    in [
        getScenicScoreFromLeft heightMap coords treeHeight
        , getScenicScoreFromRight heightMap coords treeHeight
        , getScenicScoreFromTop heightMap coords treeHeight
        , getScenicScoreFromBottom heightMap coords treeHeight
    ]

