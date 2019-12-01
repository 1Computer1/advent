module Boilerplate
    ( Sub(..)
    , mkRunner
    ) where

import Language.Haskell.TH

data Sub = A | B deriving (Show)

mkRunner :: [Int] -> DecsQ
mkRunner years = concat <$> mapM mkYear years

mkYear :: Int -> DecsQ
mkYear year = do
    let fnName = mkName "run"
        errorClause = Clause [WildP, WildP, WildP]
            (NormalB (AppE (VarE (mkName "error")) (LitE (StringL "invalid year or day")))) []
    pure
        [ SigD fnName
            (ConT (mkName "Int")
                -:> ConT (mkName "Int")
                -:> ConT (mkName "Sub")
                -:> AppT (ConT (mkName "IO")) (TupleT 0))
        , FunD fnName (concatMap (mkClauses year) [1..25] <> [errorClause])
        ]

infixr 2 -:>
(-:>) :: Type -> Type -> Type
x -:> y = AppT (AppT ArrowT x) y

mkClauses :: Int -> Int -> [Clause]
mkClauses year day =
    let yearPat = LitP (IntegerL (fromIntegral year))
        dayPat = LitP (IntegerL (fromIntegral day))
    in
        {-
            run 2019 1 A = ...
            run 2019 1 B = ...
        -}
        [ Clause [yearPat, dayPat, ConP (mkName "A") []] (NormalB (mkRun year day A)) []
        , Clause [yearPat, dayPat, ConP (mkName "B") []] (NormalB (mkRun year day B)) []
        ]

mkRun :: Int -> Int -> Sub -> Exp
mkRun year day sub =
    let solName = mkName $ "Advent.Year" <> show year <> ".Day" <> pad day <> ".solution" <> show sub
        filepath = "./input/year" <> show year <> "/day" <> pad day <> ".txt"
        output = show year <> "-12-" <> pad day <> "-" <> show sub <> ":"
    {-
        do
            input <- readFile "./input/year2019/day01.txt"
            putStrLn "2019-12-01-A:"
            runSolution Advent.Year2019.Day01.solutionA input
            putStrLn ""
    -}
    in DoE
        [ BindS (VarP (mkName "input")) (AppE (VarE (mkName "readFile")) (LitE (StringL filepath)))
        , NoBindS (AppE (VarE (mkName "putStrLn")) (LitE (StringL output)))
        , NoBindS (AppE (AppE (VarE (mkName "runSolution")) (VarE solName)) (VarE (mkName "input")))
        , NoBindS (AppE (VarE (mkName "putStrLn")) (LitE (StringL "")))
        ]

pad :: Int -> String
pad n = let s = show n in if length s == 1 then '0':s else s
