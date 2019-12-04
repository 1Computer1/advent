module Advent.Year2019.Day04 where

import Advent.Types
import Data.List
import Data.List.Split

solutionA :: Solution
solutionA = Solution $ length . uncurry (nonDecreasingCandidates hasDouble) . parse

solutionB :: Solution
solutionB = Solution $ length . uncurry (nonDecreasingCandidates hasExactDouble) . parse

parse :: String -> (Int, Int)
parse xs = let [lo, hi] = splitOn "-" xs in (read lo, read hi)

nonDecreasingCandidates :: ([Int] -> Bool) -> Int -> Int -> [Int]
nonDecreasingCandidates accepts lo hi =
    let ndLo = head $ filter nonDecreasing [lo..]
        xs = fst . break (>= hi) $ iterate (nextNonDecreasing accepts) ndLo
    in if accepts (digits ndLo)
        then xs
        else tail xs

-- n must be 6 digits, non-decreasing, and not 999999
nextNonDecreasing :: ([Int] -> Bool) -> Int -> Int
nextNonDecreasing accepts n =
    if accepts (digits result)
        then result
        else nextNonDecreasing accepts result
    where
        result = case findIndex (== 9) (tail ds) of
            Just i ->
                let incr = (ds !! i + 1) * rep (6 - i)
                    rest = n `div` 10 ^ (6 - i) * 10 ^ (6 - i)
                in incr + rest
            Nothing -> n + 1
        ds = digits n
        rep i = (10 ^ i - 1) `div` 9

nonDecreasing :: Int -> Bool
nonDecreasing = all (uncurry (<=)) . adjacents . digits

hasDouble :: [Int] -> Bool
hasDouble = any (uncurry (==)) . adjacents

hasExactDouble :: [Int] -> Bool
hasExactDouble = any (\((a, b):xs) -> null xs && a == b) . group . adjacents

adjacents :: [a] -> [(a, a)]
adjacents = zip <*> tail

-- n must be 6 digits, digits are indexed as in 012345
digits :: Int -> [Int]
digits n = map (\i -> (n `div` 10 ^ (6 - i)) `mod` 10) [1..6 :: Int]
