module TuningTrouble where

import Data.List (nub)


identifyFirstMarker :: Int -> Int -> String -> (Int, String)
identifyFirstMarker markerLength leftmostPosition buffer =
    if length buffer < markerLength
    then error $ "buffer is too short: " ++ buffer
        ++ " at position " ++ show leftmostPosition
        ++ " with marker length " ++ show markerLength
    else let inspectedBufferPart = take markerLength buffer
        in if (length $ nub inspectedBufferPart) == markerLength
            then (leftmostPosition, inspectedBufferPart)
            else identifyFirstMarker markerLength (leftmostPosition + 1) (drop 1 buffer)
    

identifyFirstStartOfPacketMarker :: String -> (Int, String)
identifyFirstStartOfPacketMarker = identifyFirstMarker 4 4


