module Advent.Runner.Types
    ( Year
    , Day
    , Part(..)
    , partFromChar
    , showPartLower
    , padDay
    ) where

type Year = Int
type Day = Int

data Part = A | B
    deriving (Show)

partFromChar :: Char -> Part
partFromChar c
    | c `elem` "Aa" = A
    | c `elem` "Bb" = B
    | otherwise     = error "unsupported part"

showPartLower :: Part -> String
showPartLower p = case p of
    A -> "a"
    B -> "b"

padDay :: Day -> String
padDay n =
    let s = show n
    in if length s == 1
        then '0':s
        else s
