{-# LANGUAGE TemplateHaskell #-}

module Advent.Year2019.Day07 where

import           Advent.Types
import           Advent.Year2019.Common.IntCode
import qualified Data.IntMap as M
import           Data.IntMap (IntMap)
import           Data.List
import           Data.List.Split

solutionA :: Solution
solutionA = Solution \input ->
    let is = parse input
        as = permutations [0..4]
        eval = flip evalProgram is
    in maximum $ flip map as \[a1, a2, a3, a4, a5] ->
        let [o1] = eval [a1, 0]
            [o2] = eval [a2, o1]
            [o3] = eval [a3, o2]
            [o4] = eval [a4, o3]
            [o5] = eval [a5, o4]
        in o5

solutionB :: Solution
solutionB = Solution \input ->
    let is = parse input
        as = permutations [5..9]
        eval = flip evalProgram is
    in maximum $ flip map as \[a1, a2, a3, a4, a5] ->
        -- Wow!
        let o1 = eval (a1:0:o5)
            o2 = eval (a2:o1)
            o3 = eval (a3:o2)
            o4 = eval (a4:o3)
            o5 = eval (a5:o4)
        in last o5

parse :: String -> IntMap Int
parse = M.fromList . zip [0..] . map read . splitOn ","
