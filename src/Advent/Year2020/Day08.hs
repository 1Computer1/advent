{-# LANGUAGE NamedFieldPuns #-}

module Advent.Year2020.Day08
    ( solutionA
    , solutionB
    ) where

import Advent.Solution
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Control.Monad.State.Strict
import Control.Applicative

data Inst = Acc | Jmp | Nop
    deriving (Eq)

toInst :: String -> Inst
toInst "acc" = Acc
toInst "jmp" = Jmp
toInst "nop" = Nop
toInst _ = error "oops"

data EvalState = EvalState
    { accum    :: Int
    , index    :: Int
    , useCount :: M.Map Int Int
    , insts    :: V.Vector (Inst, Int)
    }

solutionA :: Solution
solutionA = Solution $ \input ->
    let insts = parseInput input
        st = EvalState { accum = 0, index = 0, useCount = M.empty, insts }
    in fst $ runState evalUntilRepeat st

parseInput :: String -> V.Vector (Inst, Int)
parseInput xs = V.fromList . map parseLine $ lines xs
    where
        parseLine l =
            let [i, n] = words l
            in (toInst i, case n of '+':m -> read m; _ -> read n)

evalUntilRepeat :: State EvalState Int
evalUntilRepeat = do
    EvalState { accum, index, useCount } <- get
    case M.lookup index useCount of
        Just 1 -> pure accum
        _ -> do
            evalStep
            evalUntilRepeat

evalStep :: State EvalState ()
evalStep = do
    EvalState { accum, index, useCount, insts } <- get
    let (inst, n) = insts V.! index
    modify' (\e -> e { useCount = M.alter (\x -> succ <$> (x <|> Just 0)) index useCount })
    case inst of
        Nop -> modify' (\e -> e { index = index + 1 })
        Acc -> modify' (\e -> e { index = index + 1, accum = accum + n })
        Jmp -> modify' (\e -> e { index = index + n })

solutionB :: Solution
solutionB = Solution $ \input ->
    let base = parseInput input
        is = V.findIndices (\(x, _) -> x == Nop || x == Jmp) base
        xs = fmap (\i -> let (x, n) = base V.! i in base V.// [(i, (invert x, n))]) is
        st = fmap (\x -> EvalState { accum = 0, index = 0, useCount = M.empty, insts = x }) xs
    in fst $ runState (evalParallel (length base)) st

evalParallel :: Int -> State (V.Vector EvalState) Int
evalParallel target = do
    n <- gets V.length
    sts <- forM [0..n - 1] $ \i -> do
        st <- gets (V.! i)
        let st' = execState evalStep st
        modify' (\v -> v V.// [(i, st')])
        pure st'
    case find (\st -> index st == target) sts of
        Nothing -> evalParallel target
        Just st -> pure $ accum st

invert :: Inst -> Inst
invert Nop = Jmp
invert Jmp = Nop
invert Acc = Acc
