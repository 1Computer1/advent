module Advent.Year2020.Day03
    ( solutionA
    , solutionB
    ) where

import Advent.Solution
import Data.List

data Cell = Empty | Tree
    deriving (Enum)

toCell :: Char -> Cell
toCell '.' = Empty
toCell '#' = Tree
toCell _ = error "no"

solutionA :: Solution
solutionA = Solution $ \input ->
    let grid = map (cycle . map toCell) $ lines input
    in checkSlope 3 1 grid

checkSlope :: Int -> Int -> [[Cell]] -> Int
checkSlope right down xss = foldl' f 0 (zip [0..] yss)
    where
        f k (i, xs) = k + fromEnum (xs !! (right * i))
        yss = map snd . filter (\(i, _) -> i `mod` down == 0) $ zip [0..] xss

solutionB :: Solution
solutionB = Solution $ \input ->
    let grid = map (cycle . map toCell) $ lines input
    in product
        [ checkSlope 1 1 grid
        , checkSlope 3 1 grid
        , checkSlope 5 1 grid
        , checkSlope 7 1 grid
        , checkSlope 1 2 grid
        ]
