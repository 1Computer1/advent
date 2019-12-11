module Advent.Year2019.Day10 where

import           Advent.Types
import           Control.Monad
import           Data.List
import           Data.Ord
import qualified Data.IntSet as S
import           Data.IntSet (IntSet)

data V
    = V {-# UNPACK #-} !Int
        {-# UNPACK #-} !Int
    deriving (Eq, Ord)

-- Vector addition
infixl 6 +., -.
(+.), (-.) :: V -> V -> V
V x1 y1 +. V x2 y2 = V (x1 + x2) (y1 + y2)
V x1 y1 -. V x2 y2 = V (x1 - x2) (y1 - y2)

-- Scalar multiplication
infixl 7 @.
(@.) :: Int -> V -> V
k @. V x y = V (k * x) (k * y)

-- In clockwise direction
angle :: V -> V -> Double
angle (V x1 y1) (V x2 y2) =
    let θ = atan2 (fromIntegral y2) (fromIntegral x2) - atan2 (fromIntegral y1) (fromIntegral x1)
    in if θ < 0 then 2 * pi + θ else θ

clockwiseOf :: V -> V -> Bool
clockwiseOf (V x1 y1) (V x2 y2) = x1 * y2 - x2 * y1 >= 0

data Area = Area
    { places :: !IntSet
    , maxX   :: {-# UNPACK #-} !Int
    , maxY   :: {-# UNPACK #-} !Int
    }

solutionA :: Solution
solutionA = Solution \input ->
    let (xs, area) = parse input
    in maximum $ map (length . filterVisible area) xs

solutionB :: Solution
solutionB = Solution \input ->
    let (xs, area) = parse input
        cwAngle = angle $ V 0 (-1)
        rays = sortOn (cwAngle . fst) . maximumBy (comparing length) $ map (filterVisible area) xs
        toBeLasered = concat . transpose $ map snd rays
        V x y = toBeLasered !! 199
    in 100 * x + y

parse :: String -> ([V], Area)
parse input =
    let xs = lines input
        maxX = length $ head xs
        maxY = length xs
        places = do
            (y, row) <- zip [0..maxY] xs
            (x, obj) <- zip [0..maxX] row
            guard $ obj == '#'
            pure (V x y, cantor $ V x y)
    in (map fst places, Area (S.fromList $ map snd places) maxX maxY)

cantor :: V -> Int
cantor (V x y) = (x + y) * (x + y + 1) `div` 2 + y

filterVisible :: Area -> V -> [(V, [V])]
filterVisible area@(Area { maxX, maxY }) p@(V px py) = xs
    where
        bounds = [px, py, maxX - px, maxY - py]
        ms = concatMap similar $ directions (maximum bounds)
        xs = filter (not . null . snd) $ map (\m -> (m, filterVisibleOn area m p)) ms

filterVisibleOn :: Area -> V -> V -> [V]
filterVisibleOn area m x = filter (\v -> cantor v `S.member` places area) . takeWhile (inBounds area) $ line m x

inBounds :: Area -> V -> Bool
inBounds (Area { maxX, maxY }) (V x y) = 0 <= x && x <= maxX && 0 <= y && y <= maxY 

line :: V -> V -> [V]
line m b = map (\k -> k @. m +. b) [1..]

similar :: V -> [V]
similar v@(V vx vy) =
    if vx == 0 || vy == 0
        then [v, opp v]
        else [v, opp v, flp v, opp (flp v)]
    where
        opp (V x y) = V (-x) (-y)
        flp (V x y) = V (-x)   y

-- This is the Farey sequence adjusted to generate all coprime pairs
directions :: Int -> [V]
directions n = go 0 1 1 n []
    where
        go !a !b !c !d xs | c <= n =
            let k = (n + b) `div` d
                a' = c
                b' = d
                c' = k * c - a
                d' = k * d - b
                xs' = if a' == b'
                        then V a' b' : xs
                        else V a' b' : V b' a' : xs
            in go a' b' c' d' xs'
        go _ _ _ _ xs = V 0 1 : V 1 0 : xs
