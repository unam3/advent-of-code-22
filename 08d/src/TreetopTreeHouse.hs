module TreetopTreeHouse where

import Data.Map.Strict

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

--parseInput :: String -> Map Coords Height
--parseInput =
--        -- by y
--        . lines


--countTreesAroundEdge :: -> Int

--findInteriorVisibleTrees :: -> Int
