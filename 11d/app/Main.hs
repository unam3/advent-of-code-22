module Main where


import Control.Monad (foldM_)

import MonkeyInTheMiddle


main :: IO ()
main = readFile "testInput" >>= print . getMonkeyBusinessLevel . runNRounds id 99 . parseInput


addSuffixIfNeeded :: [String] -> String -> ([String], String)
addSuffixIfNeeded alreadyProcessed string =
    if elem string alreadyProcessed
    then addSuffixIfNeeded alreadyProcessed (string ++ "_suffix")
    else (alreadyProcessed, string)

f :: IO ()
f = foldM_
    (\ acc el ->

        do print $ "initial string: " ++ el
            
           let (acc', el') = addSuffixIfNeeded acc el

           print $ "probably modified one: " ++ el'

           pure acc'
    )
    ["b"]
    ["a", "b", "c"]
