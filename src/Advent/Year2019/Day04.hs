module Advent.Year2019.Day04 where

import Advent.Types
import Data.List
import Data.List.Split

solutionA :: Solution
solutionA = Solution $ length . uncurry (passwordCandidates hasDouble) . parse

solutionB :: Solution
solutionB = Solution $ length . uncurry (passwordCandidates hasExactDouble) . parse

parse :: String -> (Int, Int)
parse xs = let [lo, hi] = splitOn "-" xs in (read lo, read hi)

passwordCandidates :: ([(Int, Int)] -> Bool) -> Int -> Int -> [Int]
passwordCandidates accepts lo hi = takeWhile (<= hi) . tail $ iterate' (nextPassword accepts) lo

-- n must be 6 digits, non-decreasing, and not 999999
nextPassword :: ([(Int, Int)] -> Bool) -> Int -> Int
nextPassword accepts n =
    if accepts as
        then result
        else nextPassword accepts result
    where
        (result, as) = closestNonDecreasing (n + 1)

-- n must be 6 digits
closestNonDecreasing :: Int -> (Int, [(Int, Int)])
closestNonDecreasing n =
    case findIndex (\(x, y) -> x > y) as of
        Just i ->
            let d = ds !! i
                incr = d * rep (6 - i)
                rest = n `div` 10 ^ (6 - i) * 10 ^ (6 - i)
            in (incr + rest, take i as <> replicate (6 - i - 1) (d, d))
        Nothing -> (n, as)
    where
        ds = digits n
        as = adjacents ds
        rep i = (10 ^ i - 1) `div` 9

hasDouble :: [(Int, Int)] -> Bool
hasDouble = any (uncurry (==))

hasExactDouble :: [(Int, Int)] -> Bool
hasExactDouble = any (\((a, b):xs) -> null xs && a == b) . group 

adjacents :: [a] -> [(a, a)]
adjacents = zip <*> tail

-- n must be 6 digits, digits are indexed as in 012345
digits :: Int -> [Int]
digits n = map (\i -> (n `div` 10 ^ (6 - i)) `mod` 10) [1..6 :: Int]
