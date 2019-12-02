{-# LANGUAGE ExistentialQuantification #-}

module Advent.Types
    ( Solution(..)
    , runSolution
    ) where

data Solution = forall a. Show a => Solution (String -> a)

runSolution :: Solution -> String -> String
runSolution (Solution solve) = show . solve
