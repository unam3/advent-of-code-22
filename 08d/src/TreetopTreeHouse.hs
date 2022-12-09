module TreetopTreeHouse where

import Data.List (foldl')
import Data.Map.Strict (Map, (!), foldlWithKey', fromList)

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
isTreeVisibleFromLeft heigtmap coords treeHeight = False
    -- x coordinate should decrease to minCoord

isInteriorTreeVisible :: HeightMap -> Coords -> Bool
-- consecutive search: left, up, right and down
isInteriorTreeVisible heightMap coords =
    let treeHeight = (!) heightMap coords
    in isTreeVisibleFromLeft heightMap coords treeHeight

-- A tree is visible if all of the other trees between it and an edge of the grid are shorter than it
findInteriorVisibleTrees :: HeightMap -> Int
findInteriorVisibleTrees heightMap =
    let minCoord = 2
        maxCoord = subtract 1
            $ foldlWithKey' findMaxX 2 heightMap
        interiorTreeCoords = concatMap (\ y -> zip [minCoord..maxCoord] (repeat y)) [minCoord..maxCoord]
    in foldl' isInteriorTreeVisible 0 interiorTreeCoords
