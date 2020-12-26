module Advent.Year2020.Day05 where

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

solutionA :: String -> Int
solutionA input = maximum $ map findSeat xss
    where xss = map (map fromHalf) $ lines input

findSeat :: [Half] -> Int
findSeat xs = row * 8 + col
    where
        (fbs, lrs) = span (`elem` [F, B]) xs
        row = binary $ map toBit fbs
        col = binary $ map toBit lrs

binary :: [Int] -> Int
binary xs = sum $ zipWith (\i n -> 2 ^ i * n) [0..] (reverse xs)

solutionB :: String -> Int
solutionB input = n + 1
    where
        ns = sort . map (binary . map (toBit . fromHalf)) $ lines input
        Just (n, _) = find (\(x, y) -> x + 1 /= y) $ zip ns (tail ns)
