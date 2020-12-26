module Advent.Year2020.Day10
    ( solutionA
    , solutionB
    ) where

import Advent.Solution
import Data.List

solutionA :: Solution
solutionA = Solution $ \input ->
    let ns = map read $ lines input :: [Int]
        ms = 0 : sort ns <> [maximum ns + 3]
        ds = zipWith (flip (-)) ms (tail ms)
    in length (filter (== 1) ds) * length (filter (== 3) ds)

solutionB :: Solution
solutionB = Solution $ \input ->
    let ns = map read $ lines input :: [Int]
        ms = 0 : sort ns <> [maximum ns + 3]
    in countPaths ms

countPaths :: [Int] -> Int
countPaths (x:xs) = go $ (x, 1) : zip xs (repeat 0)
    where
        go [(_, k)] = k
        go ((n, k):ns) =
            let (reachable, rest) = span (\(m, _) -> m - n <= 3) ns
            in go $ map (\(m, h) -> (m, k + h)) reachable <> rest
