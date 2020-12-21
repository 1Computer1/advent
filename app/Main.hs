{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
    ( main
    ) where

import Advent.Runner.CLI
import Advent.Runner.Types
import Advent.Runner.TH
import Advent.Solution
import Control.DeepSeq
import Control.Exception
import Control.Monad
import System.Clock

getSolution :: (Year, Day, Part) -> Maybe Solution
getSolution = $(makeSolutionGetter)

measured :: IO a -> IO (Integer, a)
measured action = do
    start <- getTime Monotonic
    x <- action
    end <- getTime Monotonic
    pure (toNanoSecs $ diffTimeSpec start end, x)

main :: IO ()
main = do
    Options { year, problems } <- getOptions
    forM_ problems $ \(day, part) -> do
        let filepath = "./input/year" <> show year <> "/day" <> padDay day <> ".txt"
        let header = show year <> "-" <> padDay day <> "" <> showPartLower part <> ":"
        putStrLn header
        case getSolution (year, day, part) of
            Nothing -> putStrLn "Error: solution not found"
            Just solution -> do
                einput <- try $ readFile filepath
                case einput of
                    Left (SomeException _) -> putStrLn $ "Error: input not found at " <> filepath
                    Right input -> do
                        (ns, output) <- measured $ evaluate . force $ runSolution solution input
                        putStrLn output
                        let scale = 10 ** 6 :: Double
                        putStrLn $ "Time: " <> show (fromIntegral ns / scale) <> "ms"
        putStrLn ""
