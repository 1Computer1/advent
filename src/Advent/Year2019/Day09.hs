{-# LANGUAGE TemplateHaskell #-}

module Advent.Year2019.Day09 where

import           Advent.Types
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.List.Split
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed (Vector, (!), (!?))
import           Lens.Micro.Platform

data Memory = Memory
    { _inputs  :: [Int]
    , _memory  :: Vector Int
    , _pointer :: Int
    , _relBase :: Int
    }
    deriving (Show)

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
    | RelOffset Int
    | Halt

solutionA :: Solution
solutionA = Solution $ runProgram [1] . parse

solutionB :: Solution
solutionB = Solution $ runProgram [2] . parse

parse :: String -> Vector Int
parse = V.fromList . map read . splitOn ","

runProgram :: [Int] -> Vector Int -> [Int]
runProgram inp xs = flip appEndo [] . execWriter . evalStateT execute $ Memory inp xs 0 0

execute :: Program ()
execute = do
    op <- readOp
    case op of
        Halt -> pure ()
        _    -> runOp op >> execute

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
    RelOffset x -> relBase %= (+ x)
    Halt        -> pure ()

infix 4 &=
(&=) :: Int -> Int -> Program ()
i &= x = resizeMemory i >> memory . ix i .= x

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
    9 -> RelOffset <$> v
    99 -> pure Halt
    _ -> error $ "unknown op " <> show n
    where
        v :: Program Int
        v = do
            i <- use pointer
            r <- use relBase
            move
            case ms !! (i - i0 - 1) of
                0 -> index i >>= index
                1 -> index i
                2 -> index i >>= index . (+ r)
                m -> error $ "unknown mode " <> show m

        p :: Program Int
        p = do
            i <- use pointer
            r <- use relBase
            move
            case ms !! (i - i0 - 1) of
                0 -> index i
                1 -> index i
                2 -> (+ r) <$> index i
                m -> error $ "unknown mode " <> show m

        index :: Int -> Program Int
        index i = do
            xs <- use memory
            pure $ maybe 0 id (xs !? i)

resizeMemory :: Int -> Program ()
resizeMemory i = memory %= ensureLargeEnough i

ensureLargeEnough :: Int -> Vector Int -> Vector Int
ensureLargeEnough i v = v <> V.replicate (i - V.length v + 1) 0

-- Digits are indexed as in 43210
digits :: Int -> [Int]
digits n = map (\i -> (n `div` 10 ^ i) `mod` 10) [0..5 :: Int]

singleton :: a -> DList a
singleton x = Endo ([x] <>)
