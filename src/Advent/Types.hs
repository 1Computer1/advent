module Advent.Types
    ( Solution(..)
    , runSolution
    ) where

data Solution = forall a b. Show b => Solution
    { parse :: String -> a
    , solve :: a -> b
    }

runSolution :: Solution -> String -> IO ()
runSolution (Solution { parse, solve }) = print . solve . parse
