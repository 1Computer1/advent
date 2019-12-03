module Advent.Year2018.Day08 where

import           Advent.Types
import           Control.Arrow (first)
import           Data.List (mapAccumL, splitAt)
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import           Data.Tree

solutionA :: Solution
solutionA = Solution $ foldTree (\mm vs -> weightedSum mm + sum vs) . parse

solutionB :: Solution
solutionB = Solution $ nodeValue . parse

parse :: String -> Tree (Map Int Int)
parse = snd . go . map read . words
    where
        go (nChild:nMeta:xs) = (xs', Node mm ts)
            where
                mm = M.fromListWith (+) . map (, 1) $ ms
                ((ms, xs'), ts) = first (splitAt nMeta) parseChildren
                parseChildren = mapAccumL (const . go) xs [1..nChild]
        go _ = error "invalid input"

weightedSum :: Map Int Int -> Int
weightedSum = sum . M.elems . M.mapWithKey (*)

nodeValue :: Tree (Map Int Int) -> Int
nodeValue (Node mm []) = weightedSum mm
nodeValue (Node mm ts) = sum (map (\(i, t) -> nodeValue t * M.findWithDefault 0 i mm) (zip [1..length ts] ts))
