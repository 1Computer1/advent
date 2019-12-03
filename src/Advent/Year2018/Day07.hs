module Advent.Year2018.Day07 where

import           Advent.Types
import           Control.Applicative (liftA2)
import           Data.Char (ord)
import           Data.List (delete, insert, foldl', partition, stripPrefix)
import qualified Data.Map as M
import           Data.Map (Map)

solutionA :: Solution
solutionA = Solution $ runProcedure 1 . parse False

solutionB :: Solution
solutionB = Solution $ runProcedure 5 . parse True

data Procedure = Procedure
    { name :: Char
    , reqs :: [Char]
    , timeLeft :: Int
    } deriving (Show)

freeProcedures :: Map Char Procedure -> [Procedure]
freeProcedures = M.elems . M.filter (liftA2 (&&) (null . reqs) ((/=0) . timeLeft))

runProcedure :: Int -> Map Char Procedure -> ([Char], Int)
runProcedure workers = go [] [] 0
    where
        go prev path time pm = if null fps
            then (path, time)
            else go notFinished (path ++ map name finished) (time + 1) updatedPm
            where
                fps = if length prev == workers
                    then prev
                    else prev ++ newFps

                newFps = take (workers - length prev) (filter (not . alreadyWorking) (freeProcedures pm))
                alreadyWorking = (`elem` map name prev) . name

                (updatedFps, updatedPm) = foldr work ([], pm) fps
                work fp (fps_, pm_) = (fp':fps_, pm')
                    where
                        fp' = fp { timeLeft = timeLeft fp - 1 }
                        pm' = M.insert (name fp) fp' . M.map removeReq $ pm_
                        removeReq (p@Procedure { reqs }) = if timeLeft fp' == 0
                            then p { reqs = delete (name fp) reqs }
                            else p

                (finished, notFinished) = partition ((==0) . timeLeft) updatedFps

parse :: Bool -> String -> Map Char Procedure
parse timed = foldl' f M.empty . lines
    where
        f pm (parse_ -> (req, name)) = M.alter alterReq req . M.alter alterProc name $ pm
            where
                alterProc (Just (p@Procedure { reqs })) = Just (p { reqs = insert req reqs })
                alterProc Nothing = Just (Procedure { name, reqs = [req], timeLeft = time name })

                alterReq (Just p) = Just p
                alterReq Nothing = Just (Procedure { name = req, reqs = [], timeLeft = time req })

                time c = if timed then ord c - 4 else 1

        parse_ s0 = let
            Just (req:s1) = stripPrefix "Step " s0
            Just (name:_) = stripPrefix " must be finished before step " s1
            in (req, name)