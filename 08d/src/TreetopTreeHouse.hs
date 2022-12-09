module TreetopTreeHouse where

import Data.Map.Strict

type Coords = (Int, Int)
type Height = Int



parseY :: Int -> String -> [((Int, Int), Height)]
parseY x = zip (fmap (\ y -> (x, y)) [1..]) . fmap ((read :: String -> Int) . (:[]))

--parseInput :: String -> Map Coords Height
--parseInput =
--    fmap f
--        -- by y
--        . lines


--countTreesAroundEdge :: -> Int

--findInteriorVisibleTrees :: -> Int
