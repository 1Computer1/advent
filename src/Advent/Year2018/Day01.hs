module Advent.Year2018.Day01 where

import Advent.Types
import Data.Set (member, empty, insert)

solutionA :: Solution
solutionA = Solution $ sum . parse

solutionB :: Solution
solutionB = Solution $ firstRepeat . parse

parse :: String -> [Int]
parse = map readNum . lines
    where
        readNum ('+':xs) = read xs
        readNum xs = read xs

firstRepeat :: [Int] -> Int
firstRepeat = fst . head . dropWhile notRepeated . scanl nextResult (0, empty) . cycle
    where
        notRepeated = not . uncurry member
        nextResult (x, xs) d = (x + d, insert x xs)
