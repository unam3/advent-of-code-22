module CalorieCountingSpec where 

import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import CalorieCounting (findMostCalories, parseInput)

spec :: Spec
spec = do
    testInput <- runIO $ readFile "testInput"

    input <- runIO $ readFile "input.txt"

    describe "parseInput" $ do
        it "works"
            $ shouldBe
                (parseInput testInput)
                [[1000,2000,3000],[4000],[5000,6000],[7000,8000,9000],[10000]]

    describe "findMostCalories" $ do
        it "works"
            $ shouldBe
                (findMostCalories [[0,1], [1,2], [1,1]])
                3
        it "for for testInput"
            $ shouldBe
                (findMostCalories $ parseInput testInput)
                24000
        it "for for real input"
            $ shouldBe
                (findMostCalories $ parseInput input)
                69289
