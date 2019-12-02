module Advent.Year2018.Day05 where

import Advent.Types
import Control.Monad (mapM)
import Data.Char (toUpper)
import Data.Function (on)

solutionA :: Solution
solutionA = Solution $ length . react

solutionB :: Solution
solutionB = Solution \input ->
    let reacted = react input
    in minimum $ map (\x -> length . react $ filter (not . sameLetter x) reacted) ['a'..'z']

react :: String -> String
react = reverse . go []
    where
        go ls [] = ls
        go (l:ls) (r:rs) | oppCase l r = go ls rs
        go ls (r:rs) = go (r:ls) rs

oppCase :: Char -> Char -> Bool
oppCase x y = x /= y && toUpper x == toUpper y

sameLetter :: Char -> Char -> Bool
sameLetter = (==) `on` toUpper
