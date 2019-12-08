module Advent.Year2019.Day08 where

import Advent.Types
import Data.List
import Data.List.Split
import Data.Ord

solutionA :: Solution
solutionA = Solution \input ->
    let ls = layers 25 6 input
        l = minimumBy (comparing $ count '0') ls
    in count '1' l * count '2' l

solutionB :: Solution
solutionB = SolutionS \input ->
    let ls = layers 25 6 input
        ys = map (pixelToChar . foldMap pixelFromChar) $ transpose ls
    in unlines $ chunksOf 25 ys

data Pixel = Black | White | Trans

instance Semigroup Pixel where
    Black <> _ = Black
    White <> _ = White
    Trans <> x = x

instance Monoid Pixel where
    mempty = Trans

pixelFromChar :: Char -> Pixel
pixelFromChar = \case
    '0' -> Black
    '1' -> White
    '2' -> Trans
    _ -> error "unknown pixel"

pixelToChar :: Pixel -> Char
pixelToChar = \case
    Black -> ' '
    White -> '#'
    Trans -> ' '

layers :: Int -> Int -> [a] -> [[a]]
layers w h = chunksOf (w * h)

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)
