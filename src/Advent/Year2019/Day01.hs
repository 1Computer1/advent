module Advent.Year2019.Day01 where

import Advent.Types

solutionA :: Solution
solutionA = Solution $ sum . map requiredFuel . parse

solutionB :: Solution
solutionB = Solution $ sum . map requiredFuelTotal . parse

parse :: String -> [Int]
parse = map read . lines

requiredFuel :: Int -> Int
requiredFuel x = x `div` 3 - 2

requiredFuelTotal :: Int -> Int
requiredFuelTotal (requiredFuel -> fuel) =
    if fuel <= 0
        then 0
        else fuel + requiredFuelTotal fuel
