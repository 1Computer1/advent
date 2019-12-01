{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Advent.Year2019.Day01
import qualified Advent.Year2019.Day02
import qualified Advent.Year2019.Day03
import qualified Advent.Year2019.Day04
import qualified Advent.Year2019.Day05
import qualified Advent.Year2019.Day06
import qualified Advent.Year2019.Day07
import qualified Advent.Year2019.Day08
import qualified Advent.Year2019.Day09
import qualified Advent.Year2019.Day10
import qualified Advent.Year2019.Day11
import qualified Advent.Year2019.Day12
import qualified Advent.Year2019.Day13
import qualified Advent.Year2019.Day14
import qualified Advent.Year2019.Day15
import qualified Advent.Year2019.Day16
import qualified Advent.Year2019.Day17
import qualified Advent.Year2019.Day18
import qualified Advent.Year2019.Day19
import qualified Advent.Year2019.Day20
import qualified Advent.Year2019.Day21
import qualified Advent.Year2019.Day22
import qualified Advent.Year2019.Day23
import qualified Advent.Year2019.Day24
import qualified Advent.Year2019.Day25

import           Advent.Types
import           Boilerplate
import           Data.Char
import           System.Environment

mkRunner [2019]

usage :: String
usage = "Usage: advent <year> [day{A,B}...]"

main :: IO ()
main = do
    args <- getArgs
    case args of
        []  -> putStrLn usage
        [_] -> putStrLn usage
        (y:xs) -> mapM_ (uncurry (run (read y)) . parseProblem) xs

parseProblem :: String -> (Int, Sub)
parseProblem xs =
    case span isDigit xs of
        (day, sub:_) ->
            (read day, case toLower sub of 'a' -> A; 'b' -> B; _ -> error "could not parse subproblem")
        _ -> error "could not parse problem"
