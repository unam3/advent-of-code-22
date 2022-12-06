module TuningTrouble where

import Data.List (nub)


identifyFirstPosition' ::  Int -> String -> (Int, String)
identifyFirstPosition' leftmostPosition (c1 : c2 : c3 : c4 : restBuffer) =
    case length $ nub [c1, c2, c3, c4] of
        4 -> (leftmostPosition, [c1, c2, c3, c4])
        _ -> identifyFirstPosition' (leftmostPosition + 1) (c2 : c3 : c4 : restBuffer)

identifyFirstPosition' leftmostPosition nonMatchedInput =
    error $ "non matched input: " ++ nonMatchedInput ++ " at position " ++ show leftmostPosition

identifyFirstPosition :: String -> (Int, String)
identifyFirstPosition = identifyFirstPosition' 4
