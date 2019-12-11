module Advent.Year2019.Day02 where

import           Advent.Types
import           Advent.Year2019.Common.IntCode
import qualified Data.IntMap as M
import           Data.IntMap (IntMap)
import           Data.List
import           Data.List.Split
import           Lens.Micro.Platform

solutionA :: Solution
solutionA = Solution $ execReplacing 12 2 . parse

solutionB :: Solution
solutionB = Solution \input ->
    let xs = parse input
        choices = (,) <$> [1..99] <*> [1..99]
        target = 19690720
        Just (noun, verb) = find (\(x, y) -> execReplacing x y xs == target) choices
    in 100 * noun + verb

parse :: String -> IntMap Int
parse = M.fromList . zip [0..] . map read . splitOn ","

execReplacing :: Int -> Int -> IntMap Int -> Int
execReplacing noun verb xs = execProgram [] (xs & ix 1 .~ noun & ix 2 .~ verb) ^?! memory . ix 0
