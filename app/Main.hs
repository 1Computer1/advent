{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
    ( main
    ) where

import Advent.Runner.CLI
import Advent.Runner.Types
import Advent.Runner.TH
import Control.DeepSeq
import Control.Exception
import Control.Monad
import System.Clock

getSolution :: (Year, Day, Part) -> (forall a. Show a => Maybe (String -> a) -> r) -> r
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
        getSolution (year, day, part) $ \msolve ->
            case msolve of
                Nothing -> putStrLn "Error: solution not found"
                Just solve -> do
                    einput <- try $ readFile filepath
                    case einput of
                        Left (SomeException _) -> putStrLn $ "Error: input not found at " <> filepath
                        Right input -> do
                            (ns, output) <- measured $ evaluate . force . show $ solve input
                            putStrLn output
                            let scale = 10 ** 6 :: Double
                            putStrLn $ "Time: " <> show (fromIntegral ns / scale) <> "ms"
        putStrLn ""
