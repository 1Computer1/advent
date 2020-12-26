module Advent.Year2020.Day01 where

import Data.List

solutionA :: String -> Int
solutionA input = x' * y'
    where
        xs = map read $ lines input
        Just (x', y') = find (\(x, y) -> x + y == 2020) ((,) <$> xs <*> xs)

solutionB :: String -> Int
solutionB input = x' * y' * z'
    where
        xs = map read $ lines input
        Just (x', y', z') = find (\(x, y, z) -> x + y + z == 2020) ((,,) <$> xs <*> xs <*> xs)
