{-# LANGUAGE TemplateHaskell #-}

module Advent.Year2019.Common.IntCode
    ( Machine(..)
    , inputs
    , memory
    , pointer
    , relBase
    , runProgram
    , execProgram
    , evalProgram
    ) where

import           Control.Monad.Fail
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.DList as D
import           Data.DList (DList)
import qualified Data.IntMap as M
import           Data.IntMap (IntMap, (!))
import           Lens.Micro.Platform

data Machine = Machine
    { _inputs  :: [Int] -- Input reading needs to be lazy for day 7
    , _memory  :: !(IntMap Int)
    , _pointer :: {-# UNPACK #-} !Int
    , _relBase :: {-# UNPACK #-} !Int
    }
    deriving (Show)

makeLenses ''Machine

newtype Program a = Program { unProgram :: StateT Machine (Writer (DList Int)) a }
    deriving (Functor, Applicative, Monad, MonadState Machine, MonadWriter (DList Int))

instance MonadFail Program where
    fail = error

data Mode = Pos | Imm | Rel
    deriving (Show)

data Op a
    = Add !a !a !a
    | Mul !a !a !a
    | Inp !a
    | Out !a
    | Jnz !a !a
    | Jez !a !a
    | Lt  !a !a !a
    | Eq  !a !a !a
    | Rbo !a
    | End
    deriving (Show)

runProgram :: [Int] -> IntMap Int -> (Machine, [Int])
runProgram inp xs = fmap D.toList . runWriter $ execStateT (unProgram execute) (Machine inp xs 0 0)

execProgram :: [Int] -> IntMap Int -> Machine
execProgram inp xs = fst $ runProgram inp xs

evalProgram :: [Int] -> IntMap Int -> [Int]
evalProgram inp xs = snd $ runProgram inp xs

execute :: Program ()
execute = do
    op <- readOp
    case op of
        End -> pure ()
        _   -> runOp op >> execute

runOp :: Op Int -> Program ()
runOp op = do
    xs <- use memory
    let val j = M.findWithDefault 0 j xs
    case op of
        Add x y i -> mov i (val x + val y) >> forward 4
        Mul x y i -> mov i (val x * val y) >> forward 4
        Inp i     -> readInput >>= mov i >> forward 2
        Out x     -> tell (D.singleton $ val x) >> forward 2
        Jnz x y   -> if val x /= 0 then jump (val y) else forward 3
        Jez x y   -> if val x == 0 then jump (val y) else forward 3
        Lt  x y i -> mov i (fromEnum (val x <  val y)) >> forward 4
        Eq  x y i -> mov i (fromEnum (val x == val y)) >> forward 4
        Rbo x     -> relBase %= (+ val x) >> forward 2
        End       -> forward 1

mov :: Int -> Int -> Program ()
mov i x = memory %= M.insert i x

forward :: Int -> Program ()
forward n = pointer %= (+ n)

jump :: Int -> Program ()
jump = assign pointer

readInput :: Program Int
readInput = do
    ~(x:xs) <- use inputs -- Input reading needs to be lazy for day 7
    inputs .= xs
    pure x

readOp :: Program (Op Int)
readOp = do
    xs <- use memory
    pt <- use pointer
    rb <- use relBase
    let n = xs ! pt
        index i = M.findWithDefault 0 i xs
        par i = case digit (i + 1) n of
            -- Positional
            0 -> index (pt + i)
            -- Immediate
            1 -> pt + i
            -- Relative
            2 -> index (pt + i) + rb
            m -> error $ "unknown mode " <> show m
    pure case n `mod` 100 of
        1  -> Add (par 1) (par 2) (par 3)
        2  -> Mul (par 1) (par 2) (par 3)
        3  -> Inp (par 1)
        4  -> Out (par 1)
        5  -> Jnz (par 1) (par 2)
        6  -> Jez (par 1) (par 2)
        7  -> Lt  (par 1) (par 2) (par 3)
        8  -> Eq  (par 1) (par 2) (par 3)
        9  -> Rbo (par 1)
        99 -> End
        o  -> error $ "unknown op " <> show o

-- Digits are indexed as in 43210
digit :: Int -> Int -> Int
digit i n = (n `div` 10 ^ i) `mod` 10
