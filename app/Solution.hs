module Solution
    ( mkSolutions
    ) where

import Control.Monad
import Data.List
import Data.Maybe
import Language.Haskell.TH hiding (varE, conE, conT)
import System.Directory
import Types

getAvailableSolutions :: IO [(Int, [Int])]
getAvailableSolutions = do
    years <- map read . mapMaybe (stripPrefix "Year") <$> listDirectory "./src/Advent/"
    forM years \year -> do
        days <- map (read . take 2) . mapMaybe (stripPrefix "Day") <$> listDirectory ("./src/Advent/Year" <> show year)
        pure (year, days)

mkSolutions :: String -> DecsQ
mkSolutions name = do
    solutions <- runIO getAvailableSolutions
    let fnName = mkName name
        errorClause = Clause [WildP, WildP, WildP]
            (NormalB (conE "Nothing")) []
    pure
        [ SigD fnName $
            conT "Year"
                -:> conT "Day"
                -:> conT "Part"
                -:> conT "Maybe" $: conT "Solution"
        , FunD fnName (concatMap (uncurry mkClauses) solutions <> [errorClause])
        ]

mkClauses :: Int -> [Int] -> [Clause]
mkClauses year days = concatMap (mkClause year) days

mkClause :: Int -> Int -> [Clause]
mkClause year day =
    let yearPat = litPInt year
        dayPat = litPInt day
        mk s p = Clause [yearPat, dayPat, ConP (mkName s) []] (NormalB (mkBody year day p)) []
    in [mk "A" A, mk "B" B]

mkBody :: Int -> Int -> Part -> Exp
mkBody year day part = conE "Just"
    $. (varE $ "Advent.Year" <> show year <> ".Day" <> padDay day <> ".solution" <> show part)

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

conE :: String -> Exp
conE = ConE . mkName

conT :: String -> Type
conT = ConT . mkName

litPInt :: Integral a => a -> Pat
litPInt = LitP . IntegerL . fromIntegral
