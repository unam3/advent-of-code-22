module Main where


import Control.Monad (foldM)
import Data.List (intersperse)
import Data.Vector (fromList)

import MonkeyInTheMiddle


main :: IO ()
main = --readFile "testInput" >>= print . parseInput
    (writeInputStateForDebug
        $ runNRounds id 299 inputData_fourthMonkey_54)
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


inputData_fourthMonkey_98 :: InputData
inputData_fourthMonkey_98 = 
    fromList [
        (MultiplyBy 19, (23, 2, 3), 0, [(98, "98")]),
        (Add 6, (19, 2, 0), 0, []),
        (Sqr, (13, 1, 3), 0, []),
        (Add 3, (17, 0, 1), 0, [])
    ]

inputData_fourthMonkey_79 :: InputData
inputData_fourthMonkey_79 = 
    fromList [
        (MultiplyBy 19, (23, 2, 3), 0, [(79, "79")]),
        (Add 6, (19, 2, 0), 0, []),
        (Sqr, (13, 1, 3), 0, []),
        (Add 3, (17, 0, 1), 0, [])
    ]

inputData_fourthMonkey_54 :: InputData
inputData_fourthMonkey_54 = 
    fromList [
        (MultiplyBy 19, (23, 2, 3), 0, [(54, "54")]),
        (Add 6, (19, 2, 0), 0, []),
        (Sqr, (13, 1, 3), 0, []),
        (Add 3, (17, 0, 1), 0, [])
    ]
