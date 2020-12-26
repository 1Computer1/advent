module Advent.Year2020.Day03 where

import Data.List

data Cell = Empty | Tree
    deriving (Enum)

toCell :: Char -> Cell
toCell '.' = Empty
toCell '#' = Tree
toCell _ = error "no"

solutionA :: String -> Int
solutionA input = checkSlope 3 1 grid
    where grid = map (cycle . map toCell) $ lines input

checkSlope :: Int -> Int -> [[Cell]] -> Int
checkSlope right down xss = foldl' f 0 (zip [0..] yss)
    where
        f k (i, xs) = k + fromEnum (xs !! (right * i))
        yss = map snd . filter (\(i, _) -> i `mod` down == 0) $ zip [0..] xss

solutionB :: String -> Int
solutionB input = product
    [ checkSlope 1 1 grid
    , checkSlope 3 1 grid
    , checkSlope 5 1 grid
    , checkSlope 7 1 grid
    , checkSlope 1 2 grid
    ]
    where grid = map (cycle . map toCell) $ lines input
