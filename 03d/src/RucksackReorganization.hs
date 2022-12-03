module RucksackReorganization where

type FirstCompartment = String
type SecondCompartment = String

type Rucksack = (FirstCompartment, SecondCompartment)

--parseInput :: String -> Rucksack
parseInput = all even . fmap length . lines


