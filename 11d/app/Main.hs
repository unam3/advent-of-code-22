module Main where

import MonkeyInTheMiddle

main :: IO ()
main = readFile "testInput" >>= print . getMonkeyBusinessLevel . runNRounds id 99 . parseInput
