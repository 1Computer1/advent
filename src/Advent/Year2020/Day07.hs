{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}

module Advent.Year2020.Day07 where

import qualified Data.Bimap as M
import qualified Data.Graph.Inductive as G
import qualified Text.Parsec as P

solutionA :: String -> Int
solutionA input =
    let xs = parseInput input
        (nm, gr) = constructGraph xs
        shinygold = nm M.! "shiny gold"
        count = length . G.reachable shinygold $ G.grev gr
    in count - 1

parseInput :: String -> [(String, [(Int, String)])]
parseInput input = case P.parse pBagListings "" input of
    Left e -> error (show e)
    Right x -> x
    where
        pBagListings = pBagListing `P.sepBy` P.newline
        pBagListing = do
            bag <- pBag
            P.string " bags contain "
            xs <- pContents
            pure (bag, xs)
        pBag = do
            name1 <- P.many P.lower
            P.space
            name2 <- P.many P.lower
            pure $ name1 <> " " <> name2
        pContents = P.choice
            [ [] <$ P.string "no other bags."
            , do
                x <- flip P.sepBy (P.string ", ") $ do
                    n <- read <$> P.many P.digit
                    P.space
                    bag <- pBag
                    P.string " bag"
                    P.optional $ P.string "s"
                    pure (n, bag)
                P.string "."
                pure x
            ]

constructGraph :: [(String, [(Int, String)])] -> (M.Bimap String Int, G.Gr () Int)
constructGraph xs = (nodeMap, G.mkGraph nodes edges)
    where
        nodeMap = M.fromList . map (\(i, (x, _)) -> (x, i)) $ zip [0..] xs
        nodes = map (, ()) [0..length xs - 1]
        edges = concatMap (\(from, tos) -> map (\(w, to) -> (nodeMap M.! from, nodeMap M.! to, w)) tos) xs

solutionB :: String -> Int
solutionB input =
    let xs = parseInput input
        (nm, gr) = constructGraph xs
        shinygold = nm M.! "shiny gold"
    in countBags shinygold gr

countBags :: G.Node -> G.Gr () Int -> Int
countBags from gr = sum . map (\(_, y, w) -> w + w * countBags y gr) $ G.out gr from
