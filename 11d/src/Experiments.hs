module Experiments where


import Data.List (foldl', intersperse)
import Control.Applicative (liftA2)
import Control.Monad (void)


getX200 :: Int -> [Int]
getX200 n = foldl' (\ acc el -> acc ++ [el * n]) [] [1..1000]

f :: Int -> IO String
f n =

    do

    let list = concat . intersperse "\n" . fmap show $ getX200 n
   
    writeFile (show n) list

    pure list


ff :: [Int] -> IO [String]
ff = foldl' (\ acc n -> liftA2 (:) (f n) acc) (pure [])

-- void $ ff [13,17,19,23]
