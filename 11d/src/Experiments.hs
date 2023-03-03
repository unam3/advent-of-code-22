module Experiments where


import Data.List (foldl', intersperse)
import Control.Applicative (liftA2)
import Control.Monad (void)


getXN :: Int -> Int -> [Int]
getXN multiplier number = foldl' (\ acc el -> acc ++ [el * number]) [] [1..multiplier]

f :: Int -> IO String
f number =

    do

    let maxMultiplier = 1000

    let list = concat . intersperse "\n" . fmap show $ getXN maxMultiplier number
   
    writeFile (show number) list

    pure list


ff :: [Int] -> IO [String]
ff = foldl' (\ acc n -> liftA2 (:) (f n) acc) (pure [])

-- void $ ff [13,17,19,23]
