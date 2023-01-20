module HillClimbingAlgorithm where


import Data.HashMap.Strict (HashMap, foldlWithKey', fromList)
import Data.Maybe (fromJust)


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

-- better name?
discoverPaths' :: Heightmap -> Path -> [Path]
discoverPaths' heightmap path = undefined


getStartingSquareCoords :: Heightmap -> SquareCoords
getStartingSquareCoords =
    fromJust
        . foldlWithKey'
            (\ maybeS k v ->
                if maybeS == Nothing
                    then if v == 'S'
                        then Just k
                        else Nothing
                    else maybeS
            )
            Nothing

discoverPath :: Heightmap -> Path
discoverPath heightmap = [getStartingSquareCoords heightmap]
