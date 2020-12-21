module Advent.Year2020.Day05
    ( solutionA
    , solutionB
    ) where

import Advent.Solution
import Data.List

data Half = F | B | L | R
    deriving (Eq)

fromHalf :: Char -> Half
fromHalf 'F' = F
fromHalf 'B' = B
fromHalf 'L' = L
fromHalf 'R' = R
fromHalf _ = error "no"

toBit :: Half -> Int
toBit F = 0
toBit B = 1
toBit L = 0
toBit R = 1

solutionA :: Solution
solutionA = Solution $ \input ->
    let xss = map (map fromHalf) $ lines input
    in maximum $ map findSeat xss

findSeat :: [Half] -> Int
findSeat xs = row * 8 + col
    where
    (fbs, lrs) = span (`elem` [F, B]) xs
    row = binary $ map toBit fbs
    col = binary $ map toBit lrs

binary :: [Int] -> Int
binary xs = sum $ zipWith (\i n -> 2 ^ i * n) [0..] (reverse xs)

solutionB :: Solution
solutionB = Solution $ \input ->
    let ns = sort . map (binary . map (toBit . fromHalf)) $ lines input
        Just (n, _) = find (\(x, y) -> x + 1 /= y) $ zip ns (tail ns)
    in n + 1
