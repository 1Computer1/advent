module Advent.Year2018.Day02 where

import           Advent.Types
import           Data.List
import qualified Data.Map as M

solutionA :: Solution
solutionA = Solution $ checksum . lines

solutionB :: Solution
solutionB = Solution $ common . lines

checksum :: Ord a => [[a]] -> Int
checksum = uncurry (*) . countRepeats
    where
        countRepeats = foldl' sumRepeats (0, 0) . map hasRepeats
            where
                sumRepeats (n2, n3) (r2, r3) = (succIf r2 n2, succIf r3 n3)
                succIf b = if b then succ else id

        hasRepeats = foldl' hasRepeat (False, False) . map snd . frequencies
            where
                hasRepeat (_, r3) 2 = (True, r3)
                hasRepeat (r2, _) 3 = (r2, True)
                hasRepeat rs _ = rs

        frequencies = M.toList . M.fromListWith (+) . map (, 1 :: Int)

common :: Eq a => [[a]] -> [a]
common = map fst . filter (uncurry (==)) . uncurry zip . correctPair
    where
        correctPair xss = head [(xs, ys) | (xs, Just ys) <- map (uncurry findMatching) (searchSpace xss)]
        findMatching xs xss = (xs, find ((==1) . difference xs) xss)
        searchSpace = zip <*> drop 1 . tails
        difference xs ys = length . filter (uncurry (/=)) $ zip xs ys
