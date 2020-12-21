module Advent.Year2020.Day06
    ( solutionA
    , solutionB
    ) where

import Advent.Solution
import Data.List.Split
import qualified Data.Map as M

solutionA :: Solution
solutionA = Solution $ \input ->
    let xss = map (splitOn "\n") $ splitOn "\n\n" input
    in sum $ map countGroupOccs xss

countGroupOccs :: [String] -> Int
countGroupOccs xss = M.size $ countOccs (concat xss)

countOccs :: String -> M.Map Char Int
countOccs xs = M.fromListWith (+) $ zip xs (repeat 1)

solutionB :: Solution
solutionB = Solution $ \input ->
    let xss = map (splitOn "\n") $ splitOn "\n\n" input
    in sum $ map countGroupEvery xss

countGroupEvery :: [String] -> Int
countGroupEvery xss = M.size . M.filter (== length xss) $ countOccs (concat xss)
