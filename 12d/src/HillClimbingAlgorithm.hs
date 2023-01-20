module HillClimbingAlgorithm where


import Data.HashMap.Strict (HashMap, fromList)


type X = Int
type Y = Int

type SquareCoords = (X, Y)

type Heightmap = HashMap SquareCoords Char


assignX :: [Char] -> [(Int, Char)]
assignX = zip [0..]


assignY :: [[(Int, Char)]] -> [[(SquareCoords, Char)]]
assignY =
    zipWith
        (\ y rowXChar ->
            fmap
                (\ (x, char) -> ((x, y), char))
                rowXChar
        )
        [0..]

assignCoords :: [Char] -> [(SquareCoords, Char)]
assignCoords = concat . assignY . fmap assignX . lines

parseInput :: String -> Heightmap
parseInput = fromList . assignCoords


type Path = [SquareCoords]

