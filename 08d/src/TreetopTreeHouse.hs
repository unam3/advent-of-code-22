module TreetopTreeHouse where

import Data.Map.Strict (Map, foldlWithKey', fromList)

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

--findInteriorVisibleTrees :: -> Int
