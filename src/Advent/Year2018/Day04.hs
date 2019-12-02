module Advent.Year2018.Day04 where

import Advent.Types
import Control.Applicative (liftA2)
import Data.Char (isDigit)
import Data.Function (on)
import Data.List (groupBy, maximumBy, sort, sortBy)
import Data.Maybe (mapMaybe)

solutionA :: Solution
solutionA = Solution $ solveWith totalMinutes . parse

solutionB :: Solution
solutionB = Solution $ solveWith (snd . maxMinute) . parse

data ActionKind = Guard Int | Wake | Sleep deriving (Show, Eq)

data Action = Action
    { kind :: ActionKind
    , time :: Time
    } deriving (Show, Eq)

data Time = Time
    { month :: Int
    , day :: Int
    , hour :: Int
    , minute :: Int
    } deriving (Show, Eq, Ord)

instance Ord Action where
    compare = compare `on` time

totalMinutes :: [[Action]] -> Int
totalMinutes = sum . concatMap (map f . (zip <*> drop 1))
    where
        f (Action { kind = Sleep, time = t1 }, Action { kind = Wake, time = t2 }) = ((-) `on` minute) t2 t1
        f _ = 0

maxMinute :: [[Action]] -> (Int, Int)
maxMinute xs = maximumBy (compare `on` snd) ((zip <*> map (flip countMinuteOccurences (intervals xs))) [0..59])

countMinuteOccurences :: Int -> [(Int, Int)] -> Int
countMinuteOccurences m = sum . map (\(a, b) -> fromEnum (m >= a && m < b))

intervals :: [[Action]] -> [(Int, Int)]
intervals = concatMap (mapMaybe (uncurry toInterval) . (zip <*> drop 1))
    where
        toInterval Action { kind = Sleep, time = t1 } Action { kind = Wake, time = t2 } = Just (minute t1, minute t2)
        toInterval _  _ = Nothing

guardNum :: Action -> Int
guardNum (Action { kind = Guard n }) = n

parse :: String -> [[[Action]]]
parse = groupBy ((==) `on` guardNum . head)
            . sortBy (compare `on` guardNum . head)
            . groupBy actionEq
            . sort
            . map parse
            . lines
    where
        actionEq _ (Action { kind = Guard _ }) = False
        actionEq _ _ = True

        parse s0 = let
            '[':_:_:_:_:'-':l1:l2:'-':d1:d2:' ':h1:h2:':':m1:m2:']':' ':c:s1 = s0
            time = Time { month = read [l1, l2],
                          day = read [d1, d2],
                          hour = read [h1, h2],
                          minute = read [m1, m2] }
            in case c of
                'G' -> let
                    'u':'a':'r':'d':' ':'#':s2 = s1
                    (num, _) = span isDigit s2
                    in Action { kind = Guard (read num), time }
                'f' -> Action { kind = Sleep, time }
                'w' -> Action { kind = Wake, time }

solveWith :: ([[Action]] -> Int) -> [[[Action]]] -> Int
solveWith f = liftA2 (*) (guardNum . head . head) (fst . maxMinute) . maximumBy (compare `on` f)
