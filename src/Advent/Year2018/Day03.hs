module Advent.Year2018.Day03 where

import           Advent.Types
import           Control.Applicative (liftA2)
import           Data.Char (isDigit)
import           Data.Function (on)
import           Data.List (tails, foldl')
import qualified Data.Set as S
import           Data.Set (Set)

solutionA :: Solution
solutionA = Solution $ length . overlappingPoints . parse

solutionB :: Solution
solutionB = Solution $ head . S.elems . liftA2 S.difference S.fromDistinctAscList overlappingClaims . parse

data Claim = Claim
    { num :: Int
    , x :: Int
    , y :: Int
    , w :: Int
    , h :: Int
    }

instance Eq Claim where
    (==) = (==) `on` num

instance Ord Claim where
    compare = compare `on` num

instance Show Claim where
    show (Claim { num, x, y, w, h }) = concat ["#", show num, " @ ", show x, ",", show y, ": ", show w, "x", show h]

instance Read Claim where
    readsPrec _ s0 = let
        '#':s1 = s0
        (num, ' ':'@':' ':s2) = span isDigit s1
        (x, ',':s3) = span isDigit s2
        (y, ':':' ':s4) = span isDigit s3
        (w, 'x':s5) = span isDigit s4
        (h, s6) = span isDigit s5
        in [(Claim { num = read num, x = read x, y = read y, w = read w, h = read h }, s6)]

parse :: String -> [Claim]
parse = map read . lines

overlapsBy :: Ord a => (Claim -> [Claim] -> Set a) -> [Claim] -> Set a
overlapsBy f = foldl' g S.empty . searchSpace
    where
        searchSpace = zip <*> drop 1 . tails

        g a (c, cs) = case filter (intersects c) cs of
            [] -> a
            cs' -> S.union a (f c cs')
    
        intersects (Claim { x = x1, y = y1, w = w1, h = h1 })
                   (Claim { x = x2, y = y2, w = w2, h = h2 }) = and [x1 <= x2 + w2, x1 + w1 >= x2,
                                                                     y1 <= y2 + h2, y1 + h1 >= y2]

overlappingPoints :: [Claim] -> Set (Int, Int)
overlappingPoints = overlapsBy (\c cs -> S.unions . map (intersectionPoints c) $ cs)
    where
        intersectionPoints = S.intersection `on` S.fromList . toPoints
        toPoints (Claim { x, y, w, h }) = [(x', y') | x' <- [x..x + w - 1], y' <- [y..y + h - 1]]

overlappingClaims :: [Claim] -> Set Claim
overlappingClaims = overlapsBy (\c cs -> S.fromList (c:cs))
