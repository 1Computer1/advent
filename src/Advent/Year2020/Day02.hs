module Advent.Year2020.Day02
    ( solutionA
    , solutionB
    ) where

import Advent.Solution
import Data.Char

solutionA :: Solution
solutionA = Solution $ \input ->
    let ps = map parsePassword $ lines input
    in length $ filter validPassword ps

parsePassword :: String -> (Int, Int, Char, String)
parsePassword xs =
    let (a, '-':xs') = span isDigit xs
        (b, ' ':x:':':' ':password) = span isDigit xs'
    in (read a, read b, x, password)

validPassword :: (Int, Int, Char, String) -> Bool
validPassword (a, b, x, xs) = a <= k && k <= b
    where k = length (filter (== x) xs)

solutionB :: Solution
solutionB = Solution $ \input ->
    let ps = map parsePassword $ lines input
    in length $ filter validPassword' ps

validPassword' :: (Int, Int, Char, String) -> Bool
validPassword' (a, b, x, xs) = (xs !! (a - 1) == x) /= (xs !! (b - 1) == x)
