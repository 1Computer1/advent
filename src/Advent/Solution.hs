{-# LANGUAGE GADTs #-}

module Advent.Solution
    ( Solution(..)
    , runSolution
    ) where

data Solution where
    Solution  :: Show a => (String -> a) -> Solution
    SolutionS :: (String -> String) -> Solution

runSolution :: Solution -> String -> String
runSolution s i = case s of
    Solution  solve -> show $ solve i
    SolutionS solve -> solve i
