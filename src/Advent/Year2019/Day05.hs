module Advent.Year2019.Day05 where

import           Advent.Types
import           Control.Monad.RWS
import           Data.List
import           Data.List.Split
import qualified Data.Vector as V
import           Data.Vector (Vector, (!), (//))
import           Debug.Trace

solutionA :: Solution
solutionA = Solution $ runProgram 1 . parse

solutionB :: Solution
solutionB = Solution $ runProgram 5 . parse

data Memory = Memory
    { memory  :: Vector Int
    , pointer :: Int
    }

type Program = RWS Int [Int] Memory

data Op
    = End
    | Add
    | Mul
    | Input
    | Output
    | JumpTrue
    | JumpFalse
    | LessThan
    | Equal
    deriving (Show)

data Mode = Pos | Imm
    deriving (Show)

parse :: String -> Vector Int
parse = V.fromList . map read . splitOn ","

runProgram :: Int -> Vector Int -> [Int]
runProgram input xs = snd $ evalRWS execute input (Memory xs 0)

execute :: Program ()
execute = do
    Memory xs i <- get
    let (op, ms) = parseOpMode (xs ! i)
    runOp op ms
    case op of
        End -> pure ()
        _   -> execute

runOp :: Op -> [Mode] -> Program ()
runOp op ms = do
    case op of
        End -> pure ()
        Add -> do
            x <- valueAt 1
            y <- valueAt 2
            i <- pointerAt 3
            i .= x + y
            move 4
        Mul -> do
            x <- valueAt 1
            y <- valueAt 2
            i <- pointerAt 3
            i .= x * y
            move 4
        Input -> do
            i <- pointerAt 1
            input <- ask
            i .= input
            move 2
        Output -> do
            output <- valueAt 1
            tell [output]
            move 2
        JumpTrue -> do
            x <- valueAt 1
            i <- valueAt 2
            if x /= 0
                then jump i
                else move 3
        JumpFalse -> do
            x <- valueAt 1
            i <- valueAt 2
            if x == 0
                then jump i
                else move 3
        LessThan -> do
            x <- valueAt 1
            y <- valueAt 2
            i <- pointerAt 3
            i .= fromEnum (x < y)
            move 4
        Equal -> do
            x <- valueAt 1
            y <- valueAt 2
            i <- pointerAt 3
            i .= fromEnum (x == y)
            move 4
    where
        valueAt j = do
            Memory xs i <- get
            pure case ms !! (j - 1) of
                Pos -> xs ! (xs ! (i + j))
                Imm -> xs ! (i + j)
        pointerAt j = do
            Memory xs i <- get
            pure $ xs ! (i + j)

infix 4 .=
(.=) :: Int -> Int -> Program ()
i .= x = modify' \m -> m { memory = memory m // [(i, x)] }

relocate :: (Int -> Int) -> Program ()
relocate f = modify' \m -> m { pointer = f $ pointer m }

move :: Int -> Program ()
move = relocate . (+)

jump :: Int -> Program ()
jump i = relocate (const i)

parseOpMode :: Int -> (Op, [Mode])
parseOpMode n =
    let (d1:d2:ds) = digits n
        op = parseOp $ d1 + 10 * d2
        ms = map parseMode ds
    in (op, ms)

parseOp :: Int -> Op
parseOp = \case
    1 -> Add
    2 -> Mul
    3 -> Input
    4 -> Output
    5 -> JumpTrue
    6 -> JumpFalse
    7 -> LessThan
    8 -> Equal
    99 -> End
    n -> error $ "unknown op " <> show n

parseMode :: Int -> Mode
parseMode = \case
    0 -> Pos
    1 -> Imm
    m -> error $ "unknown mode " <> show m

-- Digits are indexed as in 543210
digits :: Int -> [Int]
digits n = map (\i -> (n `div` 10 ^ (5 - i)) `mod` 10) [5, 4..0 :: Int]

for :: [a] -> (a -> b) -> [b]
for = flip map
