{-# LANGUAGE ExistentialQuantification #-}

module Advent.Types
    ( Solution(..)
    , runSolution
    ) where

data Solution
    = forall a. Show a => Solution (String -> a)
    | SolutionS (String -> String)

runSolution :: Solution -> String -> String
runSolution s i = case s of
    Solution  solve -> show $ solve i
    SolutionS solve -> solve i
