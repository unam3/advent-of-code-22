module Experiments where


import Data.List (foldl', intersperse)
import Control.Monad (void)


getX200 :: Int -> [Int]
getX200 n = foldl' (\ acc el -> acc ++ [el * n]) [] [1..200]

f :: Int -> IO ()
f n = writeFile (show n)  (concat . intersperse "\n" . fmap show $ getX200 n)

ff :: [Int] -> IO ()
ff = foldl' (\ _ n -> void $ f n) (pure ())

