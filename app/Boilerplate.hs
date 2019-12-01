module Boilerplate
    ( Sub(..)
    , mkRunner
    ) where

import Language.Haskell.TH hiding (varE, conT)

data Sub = A | B deriving (Show)

mkRunner :: [Int] -> DecsQ
mkRunner years = concat <$> mapM mkYear years

mkYear :: Int -> DecsQ
mkYear year = do
    let fnName = mkName "run"
        errorClause = Clause [WildP, WildP, WildP]
            (NormalB (varE "error" $. litEStr "invalid year or day")) []
    pure
        [ SigD fnName $
            conT "Int"
                -:> conT "Int"
                -:> conT "Sub"
                -:> conT "IO" $: TupleT 0
        , FunD fnName (concatMap (mkClauses year) [1..25] <> [errorClause])
        ]

mkClauses :: Int -> Int -> [Clause]
mkClauses year day =
    let yearPat = litPInt year
        dayPat = litPInt day
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
    let solVar = varE $ "Advent.Year" <> show year <> ".Day" <> pad day <> ".solution" <> show sub
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
        [ BindS (VarP (mkName "input")) (varE "readFile" $. litEStr filepath)
        , NoBindS (varE "putStrLn" $. litEStr output)
        , NoBindS (varE "runSolution" $. solVar $. varE "input")
        , NoBindS (varE "putStrLn" $. litEStr "")
        ]

pad :: Int -> String
pad n = let s = show n in if length s == 1 then '0':s else s

infixr 2 -:>
(-:>) :: Type -> Type -> Type
x -:> y = AppT (AppT ArrowT x) y

infixl 3 $:
($:) :: Type -> Type -> Type
($:) = AppT

infixl 3 $.
($.) :: Exp -> Exp -> Exp
($.) = AppE

varE :: String -> Exp
varE = VarE . mkName

conT :: String -> Type
conT = ConT . mkName

litEStr :: String -> Exp
litEStr = LitE . StringL

litPInt :: Integral a => a -> Pat
litPInt = LitP . IntegerL . fromIntegral
