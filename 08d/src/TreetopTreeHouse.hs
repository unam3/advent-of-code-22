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
        in edgeTreesNumber * edgeTreesNumber
    ) . foldlWithKey' findMaxX 1


isTreeVisibleFromLeft :: HeightMap -> Coords -> Int -> Bool
isTreeVisibleFromLeft heigtMap (referenceX, _) referenceTreeHeight =
    -- x coordinate should decrease from right to left
    fst .
        L.foldl'
            (\ (areAllTreesLower, referenceTreeHeight') treeHeight ->
                (areAllTreesLower && referenceTreeHeight' > treeHeight, treeHeight)
            )
            (True, referenceTreeHeight)
        -- sort them in right order: from right to left
        . reverse
        -- get heightmap in ascending order of keys
        . elems
        -- get all the elements height to the left from Coords
        $ filterWithKey (\ (x, _) _ -> referenceX > x) heigtMap
    
isTreeVisibleFromRight :: HeightMap -> Coords -> Int -> Bool
isTreeVisibleFromRight heigtMap (referenceX, _) referenceTreeHeight =
    -- x coordinate should decrease from left to right
    fst .
        L.foldl'
            (\ (areAllTreesLower, referenceTreeHeight') treeHeight ->
                (areAllTreesLower && referenceTreeHeight' > treeHeight, treeHeight)
            )
            (True, referenceTreeHeight)
        -- get heightmap in ascending order of keys
        . elems
        -- get all the elements height to the right from Coords
        $ filterWithKey (\ (x, _) _ -> referenceX < x) heigtMap
    
isTreeVisibleFromTop :: HeightMap -> Coords -> Int -> Bool
isTreeVisibleFromTop heigtMap (_, referenceY) referenceTreeHeight =
    -- y coordinate should decrease from bottom to top
    fst .
        L.foldl'
            (\ (areAllTreesLower, referenceTreeHeight') treeHeight ->
                (areAllTreesLower && referenceTreeHeight' > treeHeight, treeHeight)
            )
            (True, referenceTreeHeight)
        -- sort them in right order: from bottom to top
        . reverse
        -- get heightmap in ascending order of keys
        . elems
        -- get all the elements height to the top from Coords
        $ filterWithKey (\ (_, y) _ -> referenceY > y) heigtMap

isTreeVisibleFromBottom :: HeightMap -> Coords -> Int -> Bool
isTreeVisibleFromBottom heigtMap (_, referenceY) referenceTreeHeight =
    -- y coordinate should decrease from top to bottom
    fst .
        L.foldl'
            (\ (areAllTreesLower, referenceTreeHeight') treeHeight ->
                (areAllTreesLower && referenceTreeHeight' > treeHeight, treeHeight)
            )
            (True, referenceTreeHeight)
        -- get heightmap in ascending order of keys
        . elems
        -- get all the elements height to the top from Coords
        $ filterWithKey (\ (_, y) _ -> referenceY < y) heigtMap

isInteriorTreeVisible :: HeightMap -> Coords -> Bool
isInteriorTreeVisible heightMap coords =
    let treeHeight = (!) heightMap coords
    -- consecutive search: left, up, right and down
    in if isTreeVisibleFromLeft heightMap coords treeHeight
    then True
    else if isTreeVisibleFromRight heightMap coords treeHeight
    then True
    else if isTreeVisibleFromTop heightMap coords treeHeight
    then True
    else isTreeVisibleFromBottom heightMap coords treeHeight


-- A tree is visible if all of the other trees between it and an edge of the grid are shorter than it
findInteriorVisibleTrees :: HeightMap -> Int
findInteriorVisibleTrees heightMap =
    -- to exclude tree from the edges
    let minCoord = 2
        maxCoord = subtract 1
            $ foldlWithKey' findMaxX 2 heightMap
        interiorTreeCoords = concatMap (\ y -> zip [minCoord..maxCoord] (repeat y)) [minCoord..maxCoord]
    in length $ filter (isInteriorTreeVisible heightMap) interiorTreeCoords
