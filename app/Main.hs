{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Advent.Year2018.Day01
import qualified Advent.Year2018.Day02
import qualified Advent.Year2018.Day03
import qualified Advent.Year2018.Day04
import qualified Advent.Year2018.Day05
import qualified Advent.Year2018.Day06
import qualified Advent.Year2018.Day07
import qualified Advent.Year2018.Day08

import qualified Advent.Year2019.Day01
import qualified Advent.Year2019.Day02
import qualified Advent.Year2019.Day03
import qualified Advent.Year2019.Day04

import           Advent.Types
import           Control.Monad
import           Interface
import           Solution
import           System.CPUTime
import           Types

mkSolutions "getSolution"

main :: IO ()
main = do
    Options { year, problems } <- getOptions
    forM_ problems \(day, part) -> do
        let filepath = "./input/year" <> show year <> "/day" <> padDay day <> ".txt"
            header = show year <> "-" <> padDay day <> "" <> showPartLower part <> ":"
        putStrLn header
        case getSolution year day part of
            Nothing -> putStrLn "unavailable solution"
            Just solution -> do
                input <- readFile filepath
                start <- getCPUTime
                putStrLn $ runSolution solution input
                end <- getCPUTime
                let diff = fromIntegral (end - start) / 10e9
                putStrLn $ "Runtime: " <> show diff <> "ms"
        putStrLn ""
