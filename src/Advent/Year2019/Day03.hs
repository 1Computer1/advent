module Advent.Year2019.Day03 where

import           Advent.Types
import           Data.List
import           Data.List.Split
import qualified Data.Set as S
import           Data.Set (Set)

solutionA :: Solution
solutionA = Solution \input ->
    let (path1, path2) = parse input
    in S.findMin . S.map magnitude $ intersections path1 path2

solutionB :: Solution
solutionB = Solution \input ->
    let (path1, path2) = parse input
    in S.findMin . S.map (\p -> walkingDistance path1 p + walkingDistance path2 p) $ intersections path1 path2

data Direction = U | D | L | R
    deriving (Show)

type Line = (Direction, Int)

type Path = [Line]

type Point = (Int, Int)

parse :: String -> (Path, Path)
parse input =
    let [l1, l2] = lines input
    in (parsePath l1, parsePath l2)
    where
        parsePath = map parseLine . splitOn ","
        parseLine xs = case uncons xs of
            Just (d, read -> n) -> case d of
                'U' -> (U, n)
                'D' -> (D, n)
                'L' -> (L, n)
                'R' -> (R, n)
                _ -> error "invalid input"
            _ -> error "invalid input"

walkingDistance :: Path -> Point -> Int
walkingDistance = go (0, 0) 0
    where
        go _ d []     _  = d
        go p d (l:ls) p0 = if inLine l p p0
            then d + distance p p0
            else go (shiftLine l p) (d + snd l) ls p0

magnitude :: Point -> Int
magnitude = distance (0, 0)

distance :: Point -> Point -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

intersections :: Path -> Path -> Set Point
intersections path1 path2 = S.delete (0, 0) $ pointsOn path1 `S.intersection` pointsOn path2  

pointsOn :: Path -> Set Point
pointsOn = fst . foldl' (uncurry go) (S.empty, (0, 0))
    where
        go acc loc line = (mkLine line loc <> acc, shiftLine line loc)

mkLine :: Line -> Point -> Set Point
mkLine (d, n) (x, y) = S.fromList $ map go [0..n]
    where
        go = case d of
            U -> \i -> (x, y + i)
            D -> \i -> (x, y - i)
            L -> \i -> (x - i, y)
            R -> \i -> (x + i, y)

inLine :: Line -> Point -> Point -> Bool
inLine (d, n) (x0, y0) (x, y) = case d of
    U -> x0 == x && y0 <= y && y <= y0 + n
    D -> x0 == x && y0 - n <= y && y <= y0
    L -> x0 - n <= x && x <= x0 && y0 == y
    R -> x0 <= x && x <= x0 + n && y0 == y

shift :: Direction -> Point -> Point
shift d = shiftLine (d, 1)

shiftLine :: Line -> Point -> Point
shiftLine (d, n) (x, y) = case d of
    U -> (x, y + n)
    D -> (x, y - n)
    L -> (x - n, y)
    R -> (x + n, y)
