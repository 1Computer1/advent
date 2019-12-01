module Advent.Year2019.Day01 where

import Advent.Types

solutionA :: Solution
solutionA = Solution
    { parse = map read . lines
    , solve = sum . map requiredFuel
    }

solutionB :: Solution
solutionB = Solution
    { parse = map read . lines
    , solve = sum . map requiredFuelTotal
    }

requiredFuel :: Int -> Int
requiredFuel x = x `div` 3 - 2

requiredFuelTotal :: Int -> Int
requiredFuelTotal (requiredFuel -> fuel) =
    if fuel <= 0
        then 0
        else fuel + requiredFuelTotal fuel
