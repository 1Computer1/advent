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
    param <- newName "t"
    cont <- newName "k"
    let errorClause = Match
            WildP
            (NormalB (AppE (VarE cont)
                (SigE
                    (ConE (mkName "Nothing"))
                    (AppT
                        (ConT (mkName "Maybe"))
                        (AppT (AppT ArrowT (ConT (mkName "String"))) (TupleT 0))))))
            []
    pure $ LamE [VarP param, VarP cont] $
        CaseE (VarE param) (concatMap (uncurry (makeYearMatches cont)) solutions <> [errorClause])

makeYearMatches :: Name -> Int -> [Int] -> [Match]
makeYearMatches cont year days = concatMap (makeDayMatches cont year) days

makeDayMatches :: Name -> Int -> Int -> [Match]
makeDayMatches cont year day =
    let yearPat = litPInt year
        dayPat = litPInt day
        partPat s = ConP (mkName s) []
        mk s p = Match (TupP [yearPat, dayPat, partPat s]) (NormalB (makeBody cont year day p)) []
    in [mk "A" A, mk "B" B]

makeBody :: Name -> Int -> Int -> Part -> Exp
makeBody cont year day part = AppE (VarE cont) $ AppE (ConE (mkName "Just")) $
    VarE . mkName $ "Advent.Year" <> show year <> ".Day" <> padDay day <> ".solution" <> show part

litPInt :: Integral a => a -> Pat
litPInt = LitP . IntegerL . fromIntegral
