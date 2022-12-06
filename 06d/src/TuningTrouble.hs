module TuningTrouble where

import Data.List (nub)


identifyFirstStartOfPacketMarker' ::  Int -> String -> (Int, String)
identifyFirstStartOfPacketMarker' leftmostPosition (c1 : c2 : c3 : c4 : restBuffer) =
    case length $ nub [c1, c2, c3, c4] of
        4 -> (leftmostPosition, [c1, c2, c3, c4])
        _ -> identifyFirstStartOfPacketMarker' (leftmostPosition + 1) (c2 : c3 : c4 : restBuffer)

identifyFirstStartOfPacketMarker' leftmostPosition nonMatchedInput =
    error $ "non matched input: " ++ nonMatchedInput ++ " at position " ++ show leftmostPosition

identifyFirstStartOfPacketMarker :: String -> (Int, String)
identifyFirstStartOfPacketMarker = identifyFirstStartOfPacketMarker' 4


