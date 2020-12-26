module Advent.Year2020.Day06 where

import Data.List.Split
import qualified Data.Map as M

solutionA :: String -> Int
solutionA input = sum $ map countGroupOccs xss
    where xss = map (splitOn "\n") $ splitOn "\n\n" input

countGroupOccs :: [String] -> Int
countGroupOccs xss = M.size $ countOccs (concat xss)

countOccs :: String -> M.Map Char Int
countOccs xs = M.fromListWith (+) $ zip xs (repeat 1)

solutionB :: String -> Int
solutionB input = sum $ map countGroupEvery xss
    where xss = map (splitOn "\n") $ splitOn "\n\n" input

countGroupEvery :: [String] -> Int
countGroupEvery xss = M.size . M.filter (== length xss) $ countOccs (concat xss)
