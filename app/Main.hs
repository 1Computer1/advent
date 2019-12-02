{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Advent.Year2019.Day01
import qualified Advent.Year2019.Day02

import           Advent.Types
import           Boilerplate
import           Data.Char
import           System.Environment

mkRunner "run"

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
