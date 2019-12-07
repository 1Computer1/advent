{-# LANGUAGE TemplateHaskell #-}

module Advent.Year2019.Day07 where

import           Advent.Types
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Functor
import           Data.List
import           Data.List.Split
import qualified Data.Vector as V
import           Data.Vector (Vector, (!))
import           Lens.Micro.Platform

data Memory = Memory
    { _inputs  :: [Int]
    , _memory  :: Vector Int
    , _pointer :: Int
    }

makeLenses ''Memory

type Program = StateT Memory (Writer (DList Int))

type DList a = Endo [a]

data Op
    = Add Int Int Int
    | Mul Int Int Int
    | Input Int
    | Output Int
    | JumpT Int Int
    | JumpF Int Int
    | Less Int Int Int
    | Equal Int Int Int
    | Halt

solutionA :: Solution
solutionA = Solution \input ->
    let is = parse input
        as = permutations [0..4]
        run = flip runProgram is
    in maximum $ flip map as \[a1, a2, a3, a4, a5] ->
        let [o1] = run [a1, 0]
            [o2] = run [a2, o1]
            [o3] = run [a3, o2]
            [o4] = run [a4, o3]
            [o5] = run [a5, o4]
        in o5

solutionB :: Solution
solutionB = Solution \input ->
    let is = parse input
        as = permutations [5..9]
        run = flip runProgram is
    in maximum $ flip map as \[a1, a2, a3, a4, a5] ->
        -- Wow!
        let o1 = run (a1:0:o5)
            o2 = run (a2:o1)
            o3 = run (a3:o2)
            o4 = run (a4:o3)
            o5 = run (a5:o4)
        in last o5

parse :: String -> Vector Int
parse = V.fromList . map read . splitOn ","

runProgram :: [Int] -> Vector Int -> [Int]
runProgram inp xs = flip appEndo [] . execWriter $ evalStateT execute (Memory inp xs 0)

execute :: Program ()
execute = do
    xs <- use memory
    i <- use pointer
    op <- readOp
    runOp op
    case op of
        Halt -> pure ()
        _    -> execute

runOp :: Op -> Program ()
runOp = \case
    Add x y i   -> i &= x + y
    Mul x y i   -> i &= x * y
    Input i     -> readInput >>= (i &=)
    Output x    -> tell $ singleton x
    JumpT x i   -> when (x /= 0) $ jump i
    JumpF x i   -> when (x == 0) $ jump i
    Less x y i  -> i &= fromEnum (x < y)
    Equal x y i -> i &= fromEnum (x == y)
    Halt        -> pure ()

infix 4 &=
(&=) :: Int -> Int -> Program ()
i &= x = memory . ix i .= x

move :: Program ()
move = pointer %= succ

jump :: Int -> Program ()
jump = assign pointer

readInput :: Program Int
readInput = do
    xs <- use inputs
    let x:xs' = xs
    inputs .= xs'
    pure x

readOp :: Program Op
readOp = do
    xs <- use memory
    i <- use pointer
    let d1:d2:ds = digits $ xs ! i
        op = d1 + 10 * d2
    move >> readOp' i op ds

readOp' :: Int -> Int -> [Int] -> Program Op
readOp' i0 n ms = case n of
    1 -> Add <$> v <*> v <*> p
    2 -> Mul <$> v <*> v <*> p
    3 -> Input <$> p
    4 -> Output <$> v
    5 -> JumpT <$> v <*> v
    6 -> JumpF <$> v <*> v
    7 -> Less <$> v <*> v <*> p
    8 -> Equal <$> v <*> v <*> p
    99 -> pure Halt
    n -> error $ "unknown op " <> show n
    where
        v = do
            xs <- use memory
            i <- use pointer
            move $> case ms !! (i - i0 - 1) of
                0 -> xs ! (xs ! i)
                1 -> xs ! i
                m -> error $ "unknown mode " <> show m
        p = do
            xs <- use memory
            i <- use pointer
            move $> xs ! i

-- Digits are indexed as in 43210
digits :: Int -> [Int]
digits n = map (\i -> (n `div` 10 ^ i) `mod` 10) [0..5 :: Int]

singleton :: a -> DList a
singleton x = Endo ([x] <>)
