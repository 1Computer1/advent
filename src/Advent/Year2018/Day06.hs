module Advent.Year2018.Day06 where

import           Advent.Types
import           Data.Char (isDigit)
import           Data.Function (on)
import           Data.List (maximumBy, minimumBy)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import           Data.Set (Set)

solutionA :: Solution
solutionA = Solution $ (maximumFiniteArea <*> borderCoords) . parse

solutionB :: Solution
solutionB = Solution $ (optimalRegion <*> borderCoords) . parse

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

borderCoords :: [(Int, Int)] -> (Int, Int, Int, Int)
borderCoords ps = let
    minX = minimum . map fst $ ps
    maxX = maximum . map fst $ ps
    minY = minimum . map snd $ ps
    maxY = maximum . map snd $ ps
    in (minX, maxX, minY, maxY)

allPoints :: (Int, Int, Int, Int) -> [(Int, Int)]
allPoints (minX, maxX, minY, maxY) = [(x, y) | x <- [minX..maxX], y <- [minY..maxY]]

generateGraph :: [(Int, Int)] -> [(Int, Int)] -> Map (Int, Int) (Int, Int)
generateGraph ps qs = Map.fromList $ mapMaybe (\q -> (q, ) <$> closestPointIn ps q) qs

closestPointIn :: [(Int, Int)] -> (Int, Int) -> Maybe (Int, Int)
closestPointIn ps q = go [] ps
    where
        dq = distance q
        go [x] [] = Just x
        go _ [] = Nothing
        go [] (x:xs) = go [x] xs
        go (m:_) (x:xs) | dq x < dq m = go [x] xs
        go acc@(m:_) (x:xs) | dq x == dq m = go (x:acc) xs
        go acc (_:xs) = go acc xs

maximumFiniteArea :: [(Int, Int)] -> (Int, Int, Int, Int) -> Int
maximumFiniteArea ps border = maximumArea . removeInfinites $ graph
    where
        graph = generateGraph ps (allPoints border)
        infinites = infiniteSourcesOf border graph
        removeInfinites = Map.filter (not . flip Set.member infinites)
        maximumArea = maximum . Map.elems . Map.fromListWith (+) . Map.foldlWithKey' (\ a _ v -> (v, 1):a) []

infiniteSourcesOf :: (Int, Int, Int, Int) -> Map (Int, Int) (Int, Int) -> Set (Int, Int)
infiniteSourcesOf (minX, maxX, minY, maxY) = Set.fromList . Map.elems . Map.filterWithKey (const . onBorder)
    where 
        onBorder p = p `elem` xEdges || p `elem` yEdges
        xEdges = [(x, y) | x <- [minX..maxX], y <- [minY, maxY]]
        yEdges = [(x, y) | x <- [minX, maxX], y <- [minY..maxY]]

optimalRegion :: [(Int, Int)] -> (Int, Int, Int, Int) -> Int
optimalRegion ps = sum . map (\q -> fromEnum $ sum (map (distance q) ps) < 10000) . allPoints

parse :: String -> [(Int, Int)]
parse = map parseLine . lines
    where
        parseLine s0 = let
            (x, ',':' ':s1) = span isDigit s0
            (y, _) = span isDigit s1
            in (read x, read y)
