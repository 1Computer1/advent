{-# LANGUAGE TemplateHaskell #-}

module Advent.Year2019.Day06 where

import           Advent.Types
import           Control.Monad.State.Strict
import qualified Data.Graph.Inductive as G
import           Data.Graph.Inductive (UGr)
import           Data.List
import           Data.List.Split
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map, (!))
import           Data.Maybe
import           Lens.Micro.Platform

data Universe = Universe
    { _graph :: UGr
    , _nodes :: Map String G.Node
    }

makeLenses ''Universe

parse :: String -> Universe
parse input = execState (build (lines input)) (Universe G.empty M.empty)
    where
        build [] = pure ()
        build (x:xs) = do
            let [parent, child] = splitOn ")" x
            parentNode <- mkNode parent
            childNode <- mkNode child
            graph %= G.insEdge (parentNode, childNode, ())
            build xs

        mkNode key = do
            Universe gr ns <- get
            if key `M.member` ns
                then pure $ ns ! key
                else do
                    let [node] = G.newNodes 1 gr
                    graph %= G.insNode (node, ())
                    nodes %= M.insert key node
                    pure node

totalOrbits :: Universe -> Int
totalOrbits (Universe gr ns) = go 0 (ns ! "COM")
    where
        go n k = case G.out gr k of
            [] -> n
            ks -> foldl' (+) n $ map (go (n + 1) . view _2) ks

findSanta :: Universe -> Int
findSanta (Universe gr ns) =
    let xs = G.rdfs [ns ! "YOU"] gr
        ys = G.rdfs [ns ! "SAN"] gr
        meet = head $ xs `intersect` ys
    in fromJust (elemIndex meet xs) + fromJust (elemIndex meet ys) - 2

solutionA :: Solution
solutionA = Solution $ totalOrbits . parse

solutionB :: Solution
solutionB = Solution $ findSanta . parse
