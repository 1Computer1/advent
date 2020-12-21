{-# LANGUAGE TemplateHaskell #-}

module Advent.Runner.TH
    ( makeSolutionGetter
    ) where

import Advent.Runner.Types
import Control.Monad
import Data.List
import Data.Maybe
import Language.Haskell.TH
import System.Directory

getAvailableSolutions :: IO [(Int, [Int])]
getAvailableSolutions = do
    years <- map read . mapMaybe (stripPrefix "Year") <$> listDirectory "./src/Advent/"
    forM years $ \year -> do
        days <- map (read . take 2) . mapMaybe (stripPrefix "Day") <$> listDirectory ("./src/Advent/Year" <> show year)
        pure (year, days)

makeSolutionGetter :: Q Exp
makeSolutionGetter  = do
    solutions <- runIO getAvailableSolutions
    let errorClause = Match WildP (NormalB (ConE (mkName "Nothing"))) []
    param <- newName "t"
    pure $ LamE [VarP param] $
        CaseE (VarE param) (concatMap (uncurry makeYearMatches) solutions <> [errorClause])

makeYearMatches :: Int -> [Int] -> [Match]
makeYearMatches year days = concatMap (makeDayMatches year) days

makeDayMatches :: Int -> Int -> [Match]
makeDayMatches year day =
    let yearPat = litPInt year
        dayPat = litPInt day
        partPat s = ConP (mkName s) []
        mk s p = Match (TupP [yearPat, dayPat, partPat s]) (NormalB (makeBody year day p)) []
    in [mk "A" A, mk "B" B]

makeBody :: Int -> Int -> Part -> Exp
makeBody year day part = AppE (ConE (mkName "Just")) $
    VarE . mkName $ "Advent.Year" <> show year <> ".Day" <> padDay day <> ".solution" <> show part

litPInt :: Integral a => a -> Pat
litPInt = LitP . IntegerL . fromIntegral
