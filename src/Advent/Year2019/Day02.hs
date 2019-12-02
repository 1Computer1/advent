module Advent.Year2019.Day02 where

import           Advent.Types
import           Control.Monad.State
import           Data.List
import           Data.List.Split
import qualified Data.Vector as V
import           Data.Vector (Vector, (!), (//))

solutionA :: Solution
solutionA = Solution
    { parse = V.fromList . map read . splitOn ","
    , solve = runReplacing 12 2
    }

solutionB :: Solution
solutionB = Solution
    { parse = V.fromList . map read . splitOn ","
    , solve = \xs ->
        let choices = (,) <$> [1..99] <*> [1..99]
            target = 19690720
            Just (noun, verb) = find (\(x, y) -> runReplacing x y xs == target) choices
        in 100 * noun + verb
    }

type Memory = Vector Int

runReplacing :: Int -> Int -> Memory -> Int
runReplacing noun verb = runProgram . replaceNounVerb noun verb

runProgram :: Memory -> Int
runProgram = V.head . execState (executeFrom 0)

executeFrom :: Int -> State Memory ()
executeFrom i = do
    xs <- get
    case xs ! i of
        1 -> run (+)
        2 -> run (*)
        99 -> pure ()
        _ -> error "something went wrong"
    where
        run op = do
            (l, r, s) <- parameters i
            modify \xs -> xs // [(s, l `op` r)]
            executeFrom (i + 4)

parameters :: Int -> State Memory (Int, Int, Int)
parameters i = do
    xs <- get
    let l = xs ! (xs ! (i + 1))
        r = xs ! (xs ! (i + 2))
        s = xs ! (i + 3)
    pure (l, r, s)

replaceNounVerb :: Int -> Int -> Memory -> Memory
replaceNounVerb noun verb xs = xs // [(1, noun), (2, verb)]
