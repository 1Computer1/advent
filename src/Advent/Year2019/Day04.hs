module Advent.Year2019.Day04 where

import Advent.Types
import Data.List
import Data.List.Split

solutionA :: Solution
solutionA = Solution $ length . filter hasDouble . uncurry nonDecreasingCandidates . parse

solutionB :: Solution
solutionB = Solution $ length . filter hasExactDouble . uncurry nonDecreasingCandidates . parse

parse :: String -> (Int, Int)
parse xs = let [lo, hi] = splitOn "-" xs in (read lo, read hi)

nonDecreasingCandidates :: Int -> Int -> [Int]
nonDecreasingCandidates lo hi = fst . break (>= hi) $ iterate nextNonDecreasing ndLo
    where
        ndLo = head $ filter nonDecreasing [lo..]

-- n must be 6 digits, non-decreasing, and not 999999
nextNonDecreasing :: Int -> Int
nextNonDecreasing n =
    case findIndex (\i -> ds !! i == 9) [1..5] of
        Just i ->
            let incr = (ds !! i + 1) * rep (6 - i)
                rest = n `div` 10 ^ (6 - i) * 10 ^ (6 - i)
            in incr + rest
        Nothing -> n + 1
    where
        ds = digits n
        rep i = (10 ^ i - 1) `div` 9

nonDecreasing :: Int -> Bool
nonDecreasing = all (uncurry (<=)) . adjacents

hasDouble :: Int -> Bool
hasDouble = any (uncurry (==)) . adjacents

hasExactDouble :: Int -> Bool
hasExactDouble = any (\((a, b):xs) -> null xs && a == b) . group . adjacents

adjacents :: Int -> [(Int, Int)]
adjacents = (zip <*> tail) . digits

-- n must be 6 digits, digits are indexed as in 012345
digits :: Int -> [Int]
digits n = map (\i -> (n `div` 10 ^ (6 - i)) `mod` 10) [1..6 :: Int]
