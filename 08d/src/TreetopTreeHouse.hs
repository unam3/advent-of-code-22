module TreetopTreeHouse where

import Data.Map.Strict

type Coords = (Int, Int)
type Height = Int



parseY :: String -> [(Int, Height)]
parseY = zip [1..] . fmap read

--parseInput :: String -> Map Coords Height
--parseInput =
--    fmap f
--        -- by y
--        . lines


--countTreesAroundEdge :: -> Int

--findInteriorVisibleTrees :: -> Int
