module Advent.Year2019.Day05 where

import           Advent.Types
import           Advent.Year2019.Common.IntCode
import qualified Data.IntMap as M
import           Data.IntMap (IntMap)
import           Data.List.Split

solutionA :: Solution
solutionA = Solution $ evalProgram [1] . parse

solutionB :: Solution
solutionB = Solution $ evalProgram [5] . parse

parse :: String -> IntMap Int
parse = M.fromList . zip [0..] . map read . splitOn ","
