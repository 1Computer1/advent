module Advent.Year2020.Day02 where

import Data.Char

solutionA :: String -> Int
solutionA input = length $ filter validPassword ps
    where ps = map parsePassword $ lines input

parsePassword :: String -> (Int, Int, Char, String)
parsePassword xs =
    let (a, '-':xs') = span isDigit xs
        (b, ' ':x:':':' ':password) = span isDigit xs'
    in (read a, read b, x, password)

validPassword :: (Int, Int, Char, String) -> Bool
validPassword (a, b, x, xs) = a <= k && k <= b
    where k = length (filter (== x) xs)

solutionB :: String -> Int
solutionB input = length $ filter validPassword' ps
    where ps = map parsePassword $ lines input

validPassword' :: (Int, Int, Char, String) -> Bool
validPassword' (a, b, x, xs) = (xs !! (a - 1) == x) /= (xs !! (b - 1) == x)
