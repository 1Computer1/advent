{-# LANGUAGE NamedFieldPuns #-}

module Advent.Year2020.Day09
    ( solutionA
    , solutionB
    ) where

import Advent.Solution
import Data.List.Split
import qualified Data.Sequence as S
import Data.Monoid

solutionA :: Solution
solutionA = Solution $ \input ->
    let ns = map read $ lines input
        windows = divvy 26 1 ns
        Just n = getFirst $ foldMap (First . invalidWindow) windows
    in n

invalidWindow :: [Int] -> Maybe Int
invalidWindow xs =
    if any (\(a, b) -> a /= b && a + b == n) ((,) <$> ns <*> ns)
        then Nothing
        else Just n
    where Just (ns, n) = unsnoc xs

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc [x] = Just ([], x)
unsnoc (x:xs) = Just (x:a, b)
    where Just (a, b) = unsnoc xs

data Slide = Slide
    { tsum :: Int
    , took :: S.Seq Int
    , more :: [Int]
    }

makeSlide :: [Int] -> Slide
makeSlide xs = Slide 0 S.empty xs

takeLess :: Slide -> Slide
takeLess s@(Slide { tsum, took, more }) = case took of
    x S.:<| xs -> Slide (tsum - x) xs more
    S.Empty -> s

takeMore :: Slide -> Slide
takeMore s@(Slide { tsum, took, more }) = case more of
    (x:xs) -> Slide (tsum + x) (took S.|> x) xs
    [] -> s

sumToN :: Int -> Slide -> S.Seq Int
sumToN target s =
    case compare (tsum s) target of
        EQ -> took s
        LT -> sumToN target (takeMore s)
        GT -> sumToN target (takeLess s)

solutionB :: Solution
solutionB = Solution $ \input ->
    let ns = map read $ lines input
        windows = divvy 26 1 ns
        Just n = getFirst $ foldMap (First . invalidWindow) windows
        ms = sumToN n (makeSlide ns)
    in minimum ms + maximum ms
