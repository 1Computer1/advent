module Advent.Year2020.Day01
    ( solutionA
    , solutionB
    ) where

import Advent.Solution
import Data.List

solutionA :: Solution
solutionA = Solution $ \input ->
    let xs = map read $ lines input :: [Int]
        Just (x', y') = find (\(x, y) -> x + y == 2020) ((,) <$> xs <*> xs)
    in x' * y'

solutionB :: Solution
solutionB = Solution $ \input ->
    let xs = map read $ lines input :: [Int]
        Just (x', y', z') = find (\(x, y, z) -> x + y + z == 2020) ((,,) <$> xs <*> xs <*> xs)
    in x' * y' * z'
