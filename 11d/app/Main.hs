module Main where


import Control.Monad (foldM)
import Data.List (intersperse)

import MonkeyInTheMiddle


main :: IO ()
main = readFile "testInput"
    >>= writeInputStateForDebug
        . runNRounds id 99
        . parseInput
            >>= print


addSuffixIfNeeded :: [String] -> String -> ([String], String)
addSuffixIfNeeded alreadyProcessed string =
    if elem string alreadyProcessed
    then addSuffixIfNeeded alreadyProcessed (string ++ "_suffix")
    else (string : alreadyProcessed, string)


writeMonkeyState :: [String] -> MonkeyState -> IO [String]
writeMonkeyState
    acc 
    (
        operation,
        (divisibleBy, throwIfTrueTo, throwIfFalseTo),
        numberOfInspectedItems,
        itemWorryStateList
    ) =
        foldM
            (\ processedItems (worryLevel, worryLevelHistory) -> do

                let initialWorryLevel = take 2 worryLevelHistory

                print $ "initial string: " ++ initialWorryLevel

                -- `grep arting` shows that all initial worry levels are 2-digit numbers

                let (processedItems', probablySuffixedWorryLevel) = addSuffixIfNeeded processedItems initialWorryLevel

                print $ "probably modified one: " ++ probablySuffixedWorryLevel

                writeFile

                    ("dump_" ++ probablySuffixedWorryLevel)
                    
                    $ concat $ intersperse
                        "\n"
                        [
                            show operation,
                            show (divisibleBy, throwIfTrueTo, throwIfFalseTo),
                            show numberOfInspectedItems,
                            show worryLevel,
                            worryLevelHistory
                        ]
                
                print processedItems'

                pure processedItems'
            )
            acc
            itemWorryStateList


writeInputStateForDebug :: InputData -> IO [String]
writeInputStateForDebug = foldM writeMonkeyState []
